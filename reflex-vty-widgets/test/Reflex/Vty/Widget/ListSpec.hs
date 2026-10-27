{-# LANGUAGE PackageImports #-}

module Reflex.Vty.Widget.ListSpec (spec) where

import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as OMap
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty.Host (defaultVtyAppConfig)
import Reflex.Vty.Widget.Layout (flex, grout, initManager_)
import Test.Hspec

import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..), gridHeight, gridRows)
import Reflex.Vty.Widget.List
import "reflex-vty" Reflex.Vty.Widget

-- | Selection, activation count, and the last activated key.
type ListObs = (Maybe Int, Int, Maybe Int)

spec :: Spec
spec = describe "Reflex.Vty.Widget.List" $ do
  it "selects the first key initially and renders rows in map order" $
    withApp defaultVtyAppConfig (20, 5) listApp $ \driver -> do
      f0 <- lastFrame driver
      frameObservation f0 `shouldBe` (Just 1, 0, Nothing)
      assertRow f0 0 ">row1"
      assertRow f0 1 " row2"
      assertRow f0 2 " row3"
      assertRow f0 3 " row4"
      assertRow f0 4 " row5"
      stopApp driver

  it "Up and Down saturate at the ends" $
    withApp defaultVtyAppConfig (20, 5) listApp $ \driver -> do
      _ <- stepInput driver (V.EvKey V.KUp [])
      fUp <- stepInput driver (V.EvKey V.KUp [])
      frameObservation fUp `shouldBe` (Just 1, 0, Nothing)
      fEnd <- pressN driver V.KDown 8
      frameObservation fEnd `shouldBe` (Just 8, 0, Nothing)
      -- The window followed the selection to the bottom.
      assertRow fEnd 4 ">row8"
      fMore <- stepInput driver (V.EvKey V.KDown [])
      frameObservation fMore `shouldBe` (Just 8, 0, Nothing)
      fStart <- pressN driver V.KUp 8
      frameObservation fStart `shouldBe` (Just 1, 0, Nothing)
      assertRow fStart 0 ">row1"
      stopApp driver

  it "Enter activates the current selection and never fires when empty" $
    withApp defaultVtyAppConfig (20, 5) listApp $ \driver -> do
      fEnter <- stepInput driver (V.EvKey V.KEnter [])
      frameObservation fEnter `shouldBe` (Just 1, 1, Just 1)
      fEmpty <- stepCommand driver Clear
      frameObservation fEmpty `shouldBe` (Nothing, 1, Just 1)
      assertRowsBlank fEmpty [0 .. 4]
      fNoAct <- stepInput driver (V.EvKey V.KEnter [])
      frameObservation fNoAct `shouldBe` (Nothing, 1, Just 1)
      fBack <- stepCommand driver Restore
      frameObservation fBack `shouldBe` (Just 1, 1, Just 1)
      assertRow fBack 0 ">row1"
      stopApp driver

  it "chooses the first key when an empty list becomes nonempty" $
    withApp defaultVtyAppConfig (20, 5) listApp $ \driver -> do
      _ <- stepCommand driver Clear
      fBack <- stepCommand driver Restore
      frameObservation fBack `shouldBe` (Just 1, 0, Nothing)
      assertRow fBack 0 ">row1"
      stopApp driver

  it "a left click selects the row under the pointer" $
    withApp defaultVtyAppConfig (20, 5) listApp $ \driver -> do
      _ <- stepInput driver (V.EvMouseDown 2 3 V.BLeft [])
      fClick <- stepInput driver (V.EvMouseUp 2 3 (Just V.BLeft))
      frameObservation fClick `shouldBe` (Just 4, 0, Nothing)
      assertRow fClick 3 ">row4"
      assertRow fClick 0 " row1"
      stopApp driver

  it "keeps the selected key when rows are inserted before it" $
    withApp defaultVtyAppConfig (20, 6) listApp $ \driver -> do
      _ <- pressN driver V.KDown 2
      fPre <- stepCommand driver (Prepend 0 "row0")
      frameObservation fPre `shouldBe` (Just 3, 0, Nothing)
      assertRow fPre 0 " row0"
      assertRow fPre 1 " row1"
      assertRow fPre 2 " row2"
      assertRow fPre 3 ">row3"
      stopApp driver

  it "reselects the nearest following key, else the nearest preceding" $
    withApp defaultVtyAppConfig (20, 5) listApp $ \driver -> do
      _ <- pressN driver V.KDown 2
      fRem <- stepCommand driver (Remove 3)
      frameObservation fRem `shouldBe` (Just 4, 0, Nothing)
      assertRow fRem 2 ">row4"
      _ <- pressN driver V.KDown 4
      fLast <- stepCommand driver (Remove 8)
      frameObservation fLast `shouldBe` (Just 7, 0, Nothing)
      assertRow fLast 4 ">row7"
      stopApp driver

  it "keeps the selected row visible in a short allotment and follows height changes" $
    withApp defaultVtyAppConfig (20, 3) listApp $ \driver -> do
      fShort <- pressN driver V.KDown 5
      frameObservation fShort `shouldBe` (Just 6, 0, Nothing)
      assertRow fShort 0 " row4"
      assertRow fShort 1 " row5"
      assertRow fShort 2 ">row6"
      -- Clicking the first visible row selects the key scrolled to it.
      _ <- stepInput driver (V.EvMouseDown 1 0 V.BLeft [])
      fClick <- stepInput driver (V.EvMouseUp 1 0 (Just V.BLeft))
      frameObservation fClick `shouldBe` (Just 4, 0, Nothing)
      assertRow fClick 0 ">row4"
      -- Growing keeps the selection and the window.
      fGrow <- resizeApp driver (20, 5)
      frameObservation fGrow `shouldBe` (Just 4, 0, Nothing)
      assertRow fGrow 0 ">row4"
      fEnd <- pressN driver V.KDown 4
      frameObservation fEnd `shouldBe` (Just 8, 0, Nothing)
      -- Shrinking follows the selection to keep it visible.
      fShrink <- resizeApp driver (20, 3)
      frameObservation fShrink `shouldBe` (Just 8, 0, Nothing)
      assertRow fShrink 0 " row6"
      assertRow fShrink 1 " row7"
      assertRow fShrink 2 ">row8"
      stopApp driver

  it "honors setSelection for present keys and ignores absent keys" $
    withApp defaultVtyAppConfig (20, 5) listApp $ \driver -> do
      fSet <- stepCommand driver (Select 5)
      frameObservation fSet `shouldBe` (Just 5, 0, Nothing)
      assertRow fSet 4 ">row5"
      fAbsent <- stepCommand driver (Select 99)
      frameObservation fAbsent `shouldBe` (Just 5, 0, Nothing)
      assertRow fAbsent 4 ">row5"
      fFar <- stepCommand driver (Select 8)
      frameObservation fFar `shouldBe` (Just 8, 0, Nothing)
      assertRow fFar 0 " row4"
      assertRow fFar 4 ">row8"
      stopApp driver

-- * Application under test

data ListCmd
  = Prepend Int T.Text
  | Remove Int
  | Clear
  | Restore
  | Select Int
  deriving (Eq, Show)

initialItems :: OMap Int T.Text
initialItems = OMap.fromList [(i, T.pack ("row" ++ show i)) | i <- [1 .. 8]]

applyCmd :: ListCmd -> OMap Int T.Text -> OMap Int T.Text
applyCmd cmd om = case cmd of
  Prepend k v -> (k, v) OMap.<| om
  Remove k -> OMap.delete k om
  Clear -> OMap.empty
  Restore -> initialItems
  Select _ -> om

-- | Eight rows, height one each, with the selected row prefixed @>@.
listApp :: TestApp ListCmd ListObs
listApp cmdEv =
  initManager_ $
    grout flex $ do
      itemsD <- foldDyn applyCmd initialItems (fforMaybe cmdEv itemCmd)
      let setE = fforMaybe cmdEv $ \case
            Select k -> Just k
            _ -> Nothing
      sl <-
        selectList
          SelectListConfig {_selectListConfig_setSelection = setE}
          itemsD
          renderRow
      let actE = _selectList_activate sl
      acts <- foldDyn (\_ n -> n + 1) (0 :: Int) actE
      lastAct <- holdDyn Nothing (Just <$> actE)
      pure ((,,) <$> _selectList_selection sl <*> acts <*> lastAct, never)
  where
    itemCmd = \case
      Prepend k v -> Just (Prepend k v)
      Remove k -> Just (Remove k)
      Clear -> Just Clear
      Restore -> Just Restore
      Select _ -> Nothing

renderRow
  :: (Reflex t, Monad m, HasImageWriter t m)
  => Int -> Dynamic t T.Text -> Dynamic t Bool -> m ()
renderRow _ vD selectedD =
  tellImages $
    ( \v sel ->
        [V.text' V.defAttr ((if sel then ">" else " ") <> v)]
    )
      <$> current vD
      <*> current selectedD

-- * Frame helpers

lastFrame :: AppDriver c o -> IO (Frame o)
lastFrame driver = do
  fs <- driverFrames driver
  case reverse fs of
    (f : _) -> pure f
    [] -> fail "expected at least one frame"

pressN :: AppDriver c o -> V.Key -> Int -> IO (Frame o)
pressN driver k n
  | n <= 0 = lastFrame driver
  | otherwise = do
      _ <- stepInput driver (V.EvKey k [])
      pressN driver k (n - 1)

rowText :: Int -> Grid -> String
rowText y g
  | y < 0 || y >= gridHeight g = ""
  | otherwise = T.unpack . T.concat . map renderSpanText $ gridRows g !! y

assertRow :: Frame o -> Int -> String -> Expectation
assertRow f y expected =
  take (length expected) (rowText y (frameGrid f)) `shouldBe` expected

assertRowsBlank :: Frame o -> [Int] -> Expectation
assertRowsBlank f ys =
  mapM_
    (\y -> T.strip (T.pack (rowText y (frameGrid f))) `shouldBe` "")
    ys
