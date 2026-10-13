{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Vty.Widget.SplitSpec (spec) where

import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec hiding (focus)

import Reflex.Vty.Host
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..))
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Split (splitH, splitV, splitVDrag)
import Reflex.Vty.Widget.Text (fill)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Text content of one row of a rendered grid.
rowText :: Grid -> Int -> String
rowText g y = T.unpack . T.concat . map renderSpanText $ gridRows g !! y

-- | All row texts of a grid.
rows :: Grid -> [String]
rows g = map (T.unpack . T.concat . map renderSpanText) (gridRows g)

-- | Index of the divider row (the only row made entirely of '-').
dividerRow :: Grid -> Int
dividerRow g = case [y | (y, r) <- zip [0 ..] (rows g), all (== '-') r && not (null r)] of
  (y : _) -> y
  [] -> error "no divider row found"

replicateRow :: Int -> Char -> String
replicateRow w c = replicate w c

mouseDownAt :: Int -> Int -> V.Event
mouseDownAt x y = V.EvMouseDown x y V.BLeft []

mouseUpAt :: Int -> Int -> V.Event
mouseUpAt x y = V.EvMouseUp x y (Just V.BLeft)

-- | Drag the divider from one row to another at a fixed column.
dragDivider :: AppDriver c o -> Int -> Int -> IO (Frame o)
dragDivider driver fromY toY = do
  _ <- stepInput driver (mouseDownAt 0 fromY)
  stepInput driver (mouseUpAt 0 toY)

----------------------------------------------------------------------------
-- Applications under test
----------------------------------------------------------------------------

-- | A draggable vertical split: pane A and pane B each fill their region
-- with their letter and report focus; the divider fills its row with '-'.
dragSplitApp :: TestApp () (T.Text, T.Text)
dragSplitApp _cmd = do
  (a, b) <- splitVDrag (fill (pure '-')) (splitPane 'A') (splitPane 'B')
  pure ((,) <$> a <*> b, never)
  where
    splitPane c = do
      fill (pure c)
      foc <- focus
      pure (ffor foc $ \f -> T.pack [c, if f then '*' else '-'])

-- | A plain vertical split whose first pane takes a third of the height.
thirdSplitApp :: TestApp () ()
thirdSplitApp _cmd = do
  _ <- splitV (pure (`div` 3)) (pure (True, True)) (fill (pure 'X')) (fill (pure 'Y'))
  pure (pure (), never)

-- | A plain horizontal split whose first pane is 8 columns narrower
-- than the viewport.
hSplitApp :: TestApp () ()
hSplitApp _cmd = do
  _ <- splitH (pure (subtract 8)) (pure (True, True)) (fill (pure 'P')) (fill (pure 'Q'))
  pure (pure (), never)

-- | A vertical split with a nested horizontal split in the second pane.
nestedSplitApp :: TestApp () ()
nestedSplitApp _cmd = do
  _ <-
    splitV (pure (subtract 4)) (pure (True, True)) (fill (pure 'T')) $
      splitH (pure (`div` 2)) (pure (True, True)) (fill (pure 'L')) (fill (pure 'R'))
  pure (pure (), never)

----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

spec :: Spec
spec = describe "Reflex.Vty.Widget.Split" $ do
  describe "splitVDrag through the production host" $ do
    it "starts with an even split, divider at half height" $
      withApp defaultVtyAppConfig (20, 10) dragSplitApp $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f : _) -> do
            rows (frameGrid f)
              `shouldBe` replicate 5 (replicateRow 20 'A') ++ [replicateRow 20 '-'] ++ replicate 4 (replicateRow 20 'B')
            dividerRow (frameGrid f) `shouldBe` 5
            frameObservation f `shouldBe` ("A*", "B-")
          [] -> expectationFailure "expected an initial frame"
        stopApp driver

    it "moves the divider on drag and keeps the fraction on resize" $
      withApp defaultVtyAppConfig (20, 10) dragSplitApp $ \driver -> do
        -- Press on the divider itself: nothing moves yet.
        f1 <- stepInput driver (mouseDownAt 0 5)
        dividerRow (frameGrid f1) `shouldBe` 5
        -- Release on row 7: the divider moves to row 7.
        f2 <- stepInput driver (mouseUpAt 0 7)
        dividerRow (frameGrid f2) `shouldBe` 7
        rows (frameGrid f2)
          `shouldBe` replicate 7 (replicateRow 20 'A') ++ [replicateRow 20 '-'] ++ replicate 2 (replicateRow 20 'B')
        frameObservation f2 `shouldBe` ("A*", "B-")
        -- The user-chosen fraction (7/10) stays after a resize: 0.7 * 20 = 14.
        resizeApp driver (20, 20)
        fs <- driverFrames driver
        case reverse fs of
          (f3 : _) -> do
            dividerRow (frameGrid f3) `shouldBe` 14
            rows (frameGrid f3)
              `shouldBe` replicate 14 (replicateRow 20 'A') ++ [replicateRow 20 '-'] ++ replicate 5 (replicateRow 20 'B')
          [] -> expectationFailure "expected a frame after resize"
        stopApp driver

    it "supports repeated drags, each moving the divider again" $
      withApp defaultVtyAppConfig (20, 10) dragSplitApp $ \driver -> do
        f1 <- dragDivider driver 5 7
        dividerRow (frameGrid f1) `shouldBe` 7
        -- A second drag starts from the divider's new position.
        f2 <- dragDivider driver 7 2
        dividerRow (frameGrid f2) `shouldBe` 2
        rows (frameGrid f2)
          `shouldBe` replicate 2 (replicateRow 20 'A') ++ [replicateRow 20 '-'] ++ replicate 7 (replicateRow 20 'B')
        -- And a third drag back down.
        f3 <- dragDivider driver 2 9
        dividerRow (frameGrid f3) `shouldBe` 9
        stopApp driver

    it "ignores drags that do not start on the divider" $
      withApp defaultVtyAppConfig (20, 10) dragSplitApp $ \driver -> do
        -- Press inside pane A (row 0) and release inside pane B's area.
        f1 <- stepInput driver (mouseDownAt 0 0)
        dividerRow (frameGrid f1) `shouldBe` 5
        f2 <- stepInput driver (mouseUpAt 2 8)
        dividerRow (frameGrid f2) `shouldBe` 5
        rows (frameGrid f2)
          `shouldBe` replicate 5 (replicateRow 20 'A') ++ [replicateRow 20 '-'] ++ replicate 4 (replicateRow 20 'B')
        -- A click in pane A also leaves focus alone.
        frameObservation f2 `shouldBe` ("A*", "B-")
        stopApp driver

    it "clamps drags past the bottom edge and keeps extents nonnegative" $
      withApp defaultVtyAppConfig (20, 10) dragSplitApp $ \driver -> do
        f <- dragDivider driver 5 50
        let g = frameGrid f
        -- The divider is clamped to the last row; pane B gets zero height.
        dividerRow g `shouldBe` 9
        rows g `shouldBe` replicate 9 (replicateRow 20 'A') ++ [replicateRow 20 '-']
        -- The rendered grid itself never reports negative extents.
        gridWidth g `shouldBe` 20
        gridHeight g `shouldBe` 10
        length (gridRows g) `shouldBe` 10
        stopApp driver

    it "switches pane focus by clicking the panes" $
      withApp defaultVtyAppConfig (20, 10) dragSplitApp $ \driver -> do
        fs0 <- driverFrames driver
        case reverse fs0 of
          (f0 : _) -> frameObservation f0 `shouldBe` ("A*", "B-")
          [] -> expectationFailure "expected an initial frame"
        -- Click inside pane B (rows 6..9): focus moves to B.
        f1 <- stepInput driver (mouseDownAt 0 6)
        frameObservation f1 `shouldBe` ("A-", "B*")
        -- Release completes the click so pane A's mouse tracking resets.
        _ <- stepInput driver (mouseUpAt 0 6)
        -- Click inside pane A (rows 0..4): focus moves back.
        f2 <- stepInput driver (mouseDownAt 0 0)
        frameObservation f2 `shouldBe` ("A*", "B-")
        _ <- stepInput driver (mouseUpAt 0 0)
        stopApp driver

    it "works at a one-row height and splits evenly after a resize" $
      withApp defaultVtyAppConfig (20, 1) dragSplitApp $ \driver -> do
        fs0 <- driverFrames driver
        case reverse fs0 of
          (f0 : _) -> do
            -- The terminal has one row: the divider occupies that row.
            rows (frameGrid f0) `shouldBe` [replicateRow 20 '-']
            gridHeight (frameGrid f0) `shouldBe` 1
            frameObservation f0 `shouldBe` ("A*", "B-")
          [] -> expectationFailure "expected an initial frame"
        -- No user fraction exists yet, so the grown terminal halves evenly.
        resizeApp driver (20, 10)
        fs <- driverFrames driver
        case reverse fs of
          (f1 : _) -> do
            dividerRow (frameGrid f1) `shouldBe` 5
            rows (frameGrid f1)
              `shouldBe` replicate 5 (replicateRow 20 'A') ++ [replicateRow 20 '-'] ++ replicate 4 (replicateRow 20 'B')
          [] -> expectationFailure "expected a frame after resize"
        stopApp driver

  describe "splitV" $ do
    it "sizes the first pane with the given function and follows resizes" $
      withApp defaultVtyAppConfig (20, 10) thirdSplitApp $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f : _) -> do
            rows (frameGrid f)
              `shouldBe` replicate 3 (replicateRow 20 'X') ++ replicate 7 (replicateRow 20 'Y')
          [] -> expectationFailure "expected an initial frame"
        -- 16 div 3 = 5 rows for X.
        resizeApp driver (20, 16)
        fs' <- driverFrames driver
        case reverse fs' of
          (f : _) ->
            rows (frameGrid f)
              `shouldBe` replicate 5 (replicateRow 20 'X') ++ replicate 11 (replicateRow 20 'Y')
          [] -> expectationFailure "expected a frame after resize"
        stopApp driver

  describe "splitH" $ do
    it "sizes the left pane with the given function" $
      withApp defaultVtyAppConfig (20, 10) hSplitApp $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f : _) ->
            rows (frameGrid f)
              `shouldBe` replicate 10 (replicate 12 'P' ++ replicate 8 'Q')
          [] -> expectationFailure "expected an initial frame"
        stopApp driver

    it "manages a nested split inside one pane" $
      withApp defaultVtyAppConfig (20, 10) nestedSplitApp $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f : _) ->
            rows (frameGrid f)
              `shouldBe` replicate 6 (replicateRow 20 'T')
                ++ replicate 4 (replicate 10 'L' ++ replicate 10 'R')
          [] -> expectationFailure "expected an initial frame"
        stopApp driver
