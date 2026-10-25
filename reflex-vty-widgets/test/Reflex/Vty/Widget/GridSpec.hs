{-# LANGUAGE PackageImports #-}
module Reflex.Vty.Widget.GridSpec (spec) where

import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Reflex.Vty.Host (defaultVtyAppConfig)
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..), gridHeight, gridRows)
import Data.Text.Zipper (fromText)
import Reflex.Vty.Widget.Grid
import Reflex.Vty.Style (Padding (..))
import Reflex.Vty.Widget.Input.Text
import "reflex-vty" Reflex.Vty.Widget
import Reflex.Vty.Widget.Layout

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

  it "nested pane renders six cells at parent-local origins and targets mouse clicks" $
    withApp defaultVtyAppConfig (20, 12) nestedGridApp $ \driver -> do
      f0 <- lastFrame driver
      -- Parent pane Region 3 2 14 8, grid gaps/padding 1: cells sit at
      -- parent-local (1,1)/(6,1)/(10,1)/(1,5)/(6,5)/(10,5), i.e. the
      -- absolute origins below. The parent origin must be applied
      -- exactly once (by pane), so cell coordinates stay local.
      charAt 4 3 (frameGrid f0) `shouldBe` Just 'A'
      charAt 9 3 (frameGrid f0) `shouldBe` Just 'B'
      charAt 13 3 (frameGrid f0) `shouldBe` Just 'C'
      charAt 4 7 (frameGrid f0) `shouldBe` Just 'D'
      charAt 9 7 (frameGrid f0) `shouldBe` Just 'E'
      charAt 13 7 (frameGrid f0) `shouldBe` Just 'F'
      -- Only the blank cell may appear at the doubled-offset position
      -- a parent-relative + viewport-relative mix would produce.
      charAt 7 5 (frameGrid f0) `shouldBe` Just ' '
      -- A click inside cell E must reach E's field through the
      -- translation chain, not be lost by a double-applied origin.
      _ <- stepInput driver (V.EvMouseDown 10 8 V.BLeft [])
      _ <- stepInput driver (V.EvMouseUp 10 8 (Just V.BLeft))
      fz <- stepInput driver (V.EvKey (V.KChar 'z') [])
      frameObservation fz `shouldBe` "ABCDEzF"
      charAt 9 7 (frameGrid fz) `shouldBe` Just 'E'
      charAt 10 7 (frameGrid fz) `shouldBe` Just 'z'
      -- Same for bottom-row cell D.
      _ <- stepInput driver (V.EvMouseDown 5 8 V.BLeft [])
      _ <- stepInput driver (V.EvMouseUp 5 8 (Just V.BLeft))
      fy <- stepInput driver (V.EvKey (V.KChar 'y') [])
      frameObservation fy `shouldBe` "ABCDyEzF"
      charAt 4 7 (frameGrid fy) `shouldBe` Just 'D'
      charAt 5 7 (frameGrid fy) `shouldBe` Just 'y'
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

-- | Character at absolute screen coordinates, if any.
charAt :: Int -> Int -> Grid -> Maybe Char
charAt x y g
  | y < 0 || y >= gridHeight g = Nothing
  | otherwise = case drop x (T.unpack (T.concat (map renderSpanText (gridRows g !! y)))) of
      (c : _) -> Just c
      [] -> Nothing

-- | Six labeled text inputs in a three-column grid with one-cell gaps
-- and padding 1, mounted in a parent pane at Region 3 2 14 8 of a
-- (20,12) terminal. The observation is the concatenation of the six
-- field values in child order, so a keystroke reveals which cell
-- received it.
nestedGridApp :: TestApp () T.Text
nestedGridApp _cmdEv = do
  let cfg =
        GridConfig
          { _gridConfig_columns = pure 3
          , _gridConfig_columnGap = pure 1
          , _gridConfig_rowGap = pure 1
          , _gridConfig_padding = pure (Padding 1 1 1 1)
          }
  initManager_ $ do
    tabNavigation
    pane (pure (Region 3 2 14 8)) (pure True) $
      grout flex $ do
        results <- grid cfg (map (tile flex . labeledInput) "ABCDEF")
        let values = foldr (\r acc -> liftA2 (<>) (_textInput_value r) acc) (pure "") results
        pure (values, never)
  where
    labeledInput c =
      textInput def {_textInputConfig_initialValue = fromText (T.singleton c)}
