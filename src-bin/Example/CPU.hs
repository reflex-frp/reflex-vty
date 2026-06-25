-- |
--   Description: A CPU usage indicator
module Example.CPU where

import Control.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Ratio
import qualified Data.Text as T
import Data.Time
import Data.Word
import qualified Graphics.Vty as V
import Reflex
import Text.Printf

import Data.Text.Zipper (TextAlignment (..))
import Reflex.Vty

-- | Each constructor represents a cpu statistic column as presented in @/proc/stat@
data CpuStat
  = CpuStat_User
  | CpuStat_Nice
  | CpuStat_System
  | CpuStat_Idle
  | CpuStat_Iowait
  | CpuStat_Irq
  | CpuStat_Softirq
  | CpuStat_Steal
  | CpuStat_Guest
  | CpuStat_GuestNice
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Read @/proc/stat@
getCpuStat :: IO (Maybe (CpuStat -> Word64))
getCpuStat = do
  s <- readFile "/proc/stat"
  _ <- evaluate $ length s -- Make readFile strict
  pure $ do
    cpuSummaryLine : _ <- pure $ lines s
    [user, nice, system, idle, iowait, irq, softirq, steal, guest, guestNice] <- pure $ map read $ words $ drop 4 cpuSummaryLine
    pure $ \case
      CpuStat_User -> user
      CpuStat_Nice -> nice
      CpuStat_System -> system
      CpuStat_Idle -> idle
      CpuStat_Iowait -> iowait
      CpuStat_Irq -> irq
      CpuStat_Softirq -> softirq
      CpuStat_Steal -> steal
      CpuStat_Guest -> guest
      CpuStat_GuestNice -> guestNice

sumStats :: (CpuStat -> Word64) -> [CpuStat] -> Word64
sumStats get stats = sum $ get <$> stats

-- | user + nice + system + irq + softirq + steal
nonIdleStats :: [CpuStat]
nonIdleStats =
  [ CpuStat_User
  , CpuStat_Nice
  , CpuStat_System
  , CpuStat_Irq
  , CpuStat_Softirq
  , CpuStat_Steal
  ]

-- | idle + iowait
idleStats :: [CpuStat]
idleStats =
  [ CpuStat_Idle
  , CpuStat_Iowait
  ]

-- | Draws the cpu usage percent as a live-updating bar graph. The output should look like:
--
-- > ╔══════ CPU Usage:  38% ══════╗
-- > ║                             ║
-- > ║                             ║
-- > ║                             ║
-- > ║                             ║
-- > ║                             ║
-- > ║                             ║
-- > ║█████████████████████████████║
-- > ║█████████████████████████████║
-- > ║█████████████████████████████║
-- > ║█████████████████████████████║
-- > ╚═════════════════════════════╝
cpuStats
  :: ( Reflex t
     , MonadFix m
     , MonadHold t m
     , MonadIO (Performable m)
     , MonadIO m
     , PerformEvent t m
     , PostBuild t m
     , TriggerEvent t m
     , HasDisplayRegion t m
     , HasImageWriter t m
     , HasLayout t m
     , HasFocus t m
     , HasInput t m
     , HasFocusReader t m
     , HasTheme t m
     )
  => m ()
cpuStats = do
  tick <- tickLossy 0.25 =<< liftIO getCurrentTime
  cpuStat :: Event t (Word64, Word64) <- fmap (fmapMaybe id) $
    performEvent $
      ffor tick $ \_ -> do
        get <- liftIO getCpuStat
        pure $ case get of
          Nothing -> Nothing
          Just get' -> Just (sumStats get' nonIdleStats, sumStats get' idleStats)
  active <- foldDyn cpuPercentStep ((0, 0), 0) cpuStat
  let pct = fmap snd active
  chart pct

chart
  :: ( MonadFix m
     , MonadHold t m
     , HasFocus t m
     , HasLayout t m
     , HasImageWriter t m
     , HasInput t m
     , HasDisplayRegion t m
     , HasFocusReader t m
     , HasTheme t m
     )
  => Dynamic t (Ratio Word64) -> m ()
chart pct = do
  let title = ffor pct $ \x ->
        mconcat
          [ " CPU Usage: "
          , T.pack (printf "%3d" $ (ceiling $ x * 100 :: Int))
          , "% "
          ]
  boxTitle (pure TextAlignment_Center) (pure doubleBoxStyle) (current title) $
    grout flex $ do
      dh <- displayHeight
      dw <- displayWidth
      let grad = gradient1D [(0.0, RGB 50 205 50), (0.4, RGB 255 220 0), (0.7, RGB 255 140 0), (1.0, RGB 220 40 40)]
          chartImgs h w r =
            let filledF = fromIntegral (numerator r) * fromIntegral h / fromIntegral (denominator r) :: Double
                filled = min h $ floor filledF
                frac = filledF - fromIntegral filled
                partial = if frac > 0 && filled < h then ceiling (8 * frac) :: Int else 0
                rowColor i = V.withForeColor V.defAttr $ fromRGB $ sampleGradient1D grad (fromIntegral i / fromIntegral (max 1 (h - 1)))
                fullImgs =
                  [ V.translate 0 (h - 1 - i) $ V.charFill (rowColor i) '█' w 1
                  | i <- [0 .. filled - 1]
                  ]
                partialImg =
                  if partial > 0 && filled < h
                    then [V.translate 0 (h - 1 - filled) $ V.text' (rowColor filled) (T.replicate w (T.singleton $ eighthBlocks partial))]
                    else []
            in fullImgs ++ partialImg
      tellImages $ chartImgs <$> current dh <*> current dw <*> current pct

eighthBlocks :: (Eq a, Num a, Ord a) => a -> Char
eighthBlocks n =
  if
    | n <= 0 -> ' '
    | n == 1 -> '▁'
    | n == 2 -> '▂'
    | n == 3 -> '▃'
    | n == 4 -> '▄'
    | n == 5 -> '▅'
    | n == 6 -> '▆'
    | n == 7 -> '▇'
    | otherwise -> '█'

-- | Determine the current percentage usage according to this algorithm:
--
-- PrevIdle = previdle + previowait
-- Idle = idle + iowait
--
-- PrevNonIdle = prevuser + prevnice + prevsystem + previrq + prevsoftirq + prevsteal
-- NonIdle = user + nice + system + irq + softirq + steal
--
-- PrevTotal = PrevIdle + PrevNonIdle
-- Total = Idle + NonIdle
--
-- totald = Total - PrevTotal
-- idled = Idle - PrevIdle
--
-- CPU_Percentage = (totald - idled)/totald
--
-- Source: https://stackoverflow.com/questions/23367857/accurate-calculation-of-cpu-usage-given-in-percentage-in-linux
cpuPercentStep
  :: (Word64, Word64) -- Current active, Current idle
  -> ((Word64, Word64), Ratio Word64) -- (Previous idle, Previous total), previous percent
  -> ((Word64, Word64), Ratio Word64) -- (New idle, new total), percent
cpuPercentStep (nonidle, idle) ((previdle, prevtotal), _) =
  let total = idle + nonidle
      idled = idle - previdle
      totald = total - prevtotal
  in ( (idle, total)
     , (totald - idled) % totald
     )
