{-# LANGUAGE PackageImports #-}
module Reflex.Vty.Widget.GridSpec (spec) where

import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Reflex.Vty.Host (defaultVtyAppConfig)
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..), gridHeight, gridRows)
import Reflex.Vty.Widget.Grid
import Reflex.Vty.Style (Padding (..))
import Reflex.Vty.Widget.Input.Text
import "reflex-vty" Reflex.Vty.Widget
import Reflex.Vty.Widget.Layout
import "reflex-vty" Reflex.Vty.Widget
import Reflex.Vty.Widget.Layout.Spacing

lastFrame :: AppDriver c o -> IO (Frame o)
lastFrame driver = do
  fs <- driverFrames driver
  case reverse fs of
    (f : _) -> pure f
    [] -> fail "expected at least one frame"

spec :: Spec
spec = describe "Reflex.Vty.Widget.Grid" $ do

  it "14x8 grid of six cells, three columns, gaps 1, padding 1" $
    withApp defaultVtyAppConfig (14, 8) (gridApp False) $ \driver -> do
      f0 <- lastFrame driver
      -- Inner width 12, height 6: columns 4/3/3 at x 1/6/10, rows 3/2 at y 1/5.
      let g0 = frameGrid f0
      T.take 1 (T.drop 1 (T.pack (gridRowText 1 g0))) `shouldBe` "A"
      T.take 1 (T.drop 6 (T.pack (gridRowText 1 g0))) `shouldBe` "B"
      T.take 1 (T.drop 10 (T.pack (gridRowText 1 g0))) `shouldBe` "C"
      T.take 1 (T.drop 1 (T.pack (gridRowText 5 g0))) `shouldBe` "D"
      T.take 1 (T.drop 6 (T.pack (gridRowText 5 g0))) `shouldBe` "E"
      stopApp driver

  it "changing columns preserves child values and focus" $
    withApp defaultVtyAppConfig (14, 8) (gridApp True) $ \driver -> do
      f0 <- lastFrame driver
      frameObservation f0 `shouldBe` ""
      _ <- stepInput driver (V.EvKey (V.KChar '\t') [])
      f1 <- stepInput driver (V.EvKey (V.KChar 'a') [])
      frameObservation f1 `shouldBe` "a"
      _ <- stepCommand driver GridTwoColumns
      f2 <- stepInput driver (V.EvKey (V.KChar 'b') [])
      frameObservation f2 `shouldBe` "ab"
      stopApp driver

-- | Six labeled flex cells in a three-column grid with one-cell gaps
-- and padding 1. With @dynamic@, a command toggles two columns and the
-- cells hold text inputs; the observation is their concatenated text.
gridApp :: Bool -> TestApp GridCmd T.Text
gridApp dynamic cmdEv = do
  colsD <-
    if dynamic
      then foldDyn (const (\c -> if c == 3 then 2 else 3)) 3 $
             fforMaybe cmdEv (\c -> if c == GridTwoColumns then Just () else Nothing)
      else pure 3
  let cfg =
        GridConfig
          { _gridConfig_columns = colsD
          , _gridConfig_columnGap = pure 1
          , _gridConfig_rowGap = pure 1
          , _gridConfig_padding = pure (Padding 1 1 1 1)
          }
  initManager_ $ do
    tabNavigation
    grout flex $
      if dynamic
        then do
          vals <- gridWithInputs cfg
          pure (vals, never)
        else do
          _ <- grid cfg (zipWith (const . label) ['A','B','C','D','E','F'] [1 :: Int ..])
          pure (pure "", never)
  where
    gridWithInputs cfg = do
      results <- grid cfg (replicate 6 (tile flex (textInput def)))
      let values = foldr (\r acc -> liftA2 (<>) (_textInput_value r) acc) (pure "") results
      pure (values)

data GridCmd = GridTwoColumns
  deriving (Eq, Show)

label :: (Reflex t, Monad m, HasImageWriter t m) => Char -> m ()
label c = tellImages (pure [V.string V.defAttr [c]])

-- Local copy of the core CompositionSpec grid helper (the core spec
-- module is not shared).
gridRowText :: Int -> Grid -> String
gridRowText y g
  | y < 0 || y >= gridHeight g = ""
  | otherwise = T.unpack . T.concat . map renderSpanText $ gridRows g !! y
