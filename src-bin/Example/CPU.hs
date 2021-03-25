{-|
  Description: A CPU usage indicator
-}
module Example.CPU where

import Control.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Ratio
import Data.Time
import Data.Word
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Text.Printf

import Reflex
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
   deriving (Show, Read, Eq, Ord, Enum, Bounded)

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
--
cpuStats
  :: ( Reflex t
     , MonadFix m
     , MonadHold t m
     , MonadIO (Performable m)
     , MonadIO m
     , PerformEvent t m
     , PostBuild t m
     , TriggerEvent t m
     , HasDisplaySize t m
     , ImageWriter t m
     , MonadLayout t m
     , MonadFocus t m
     , MonadNodeId m
     , HasVtyWidgetCtx t m
     , HasVtyInput t m
     )
  => m ()
cpuStats = do
  tick <- tickLossy 0.25 =<< liftIO getCurrentTime
  cpuStat :: Event t (Word64, Word64) <- fmap (fmapMaybe id) $
    performEvent $ ffor tick $ \_ -> do
      get <- liftIO getCpuStat
      pure $ case get of
        Nothing -> Nothing
        Just get' -> Just (sumStats get' nonIdleStats, sumStats get' idleStats)
  active <- foldDyn cpuPercentStep ((0, 0), 0) cpuStat
  let pct = fmap snd active
  chart pct

chart
  :: ( MonadFix m
     , MonadFocus t m
     , MonadLayout t m
     , MonadNodeId m
     , ImageWriter t m
     , HasVtyInput t m
     , HasVtyWidgetCtx t m
     , HasDisplaySize t m
     )
  => Dynamic t (Ratio Word64) -> m ()
chart pct = do
  let title = ffor pct $ \x -> mconcat
        [ " CPU Usage: "
        , T.pack (printf "%3d" $ (ceiling $ x * 100 :: Int))
        , "% "
        ]
  boxTitle (pure doubleBoxStyle) (current title) $ col $ do
    grout flex blank
    dh <- displayHeight
    let heights = calcRowHeights <$> dh <*> pct
        quarters = fst <$> heights
        eighths = snd <$> heights
        eighthRow = ffor eighths $ \x -> if x == 0 then 0 else 1
    tile (fixed eighthRow) $ fill' (current $ eighthBlocks <$> eighths) $ current $
      ffor quarters $ \q ->
        if | _quarter_fourth q > 0 -> red
           | _quarter_third q > 0 -> orange
           | _quarter_second q > 0 -> yellow
           | otherwise -> white
    tile (fixed $ _quarter_fourth <$> quarters) $ fill' (pure '█') (pure red)
    tile (fixed $ _quarter_third <$> quarters) $ fill' (pure '█') (pure orange)
    tile (fixed $ _quarter_second <$> quarters) $ fill' (pure '█') (pure yellow)
    tile (fixed $ _quarter_first <$> quarters) $ fill' (pure '█') (pure white)
  where
    -- Calculate number of full rows, height of partial row
    calcRowHeights :: Int -> Ratio Word64 -> (Quarter Int, Int)
    calcRowHeights h r =
      let (full, leftovers) = divMod (numerator r * fromIntegral h) (denominator r)
          partial = ceiling $ 8 * (leftovers % denominator r)
          quarter = ceiling $ fromIntegral h / (4 :: Double)
          n = fromIntegral full
      in if | n <= quarter ->
                (Quarter n 0 0 0, partial)
            | n <= (2 * quarter) ->
                (Quarter quarter (n - quarter) 0 0, partial)
            | n <= (3 * quarter) ->
                (Quarter quarter quarter (n - (2 * quarter)) 0, partial)
            | otherwise ->
                (Quarter quarter quarter quarter (n - (3 * quarter)), partial)
    fill' bc attr = do
      dw <- displayWidth
      dh <- displayHeight
      let fillImg =
            (\w h c a -> [V.charFill a c w h])
            <$> current dw
            <*> current dh
            <*> bc
            <*> attr
      tellImages fillImg
    red = V.withForeColor V.defAttr $ V.rgbColor 255 0 0
    orange = V.withForeColor V.defAttr $ V.rgbColor 255 165 0
    yellow = V.withForeColor V.defAttr $ V.rgbColor 255 255 0
    white = V.withForeColor V.defAttr $ V.rgbColor 255 255 255

data Quarter a = Quarter
  { _quarter_first :: a
  , _quarter_second :: a
  , _quarter_third :: a
  , _quarter_fourth :: a
  }

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
