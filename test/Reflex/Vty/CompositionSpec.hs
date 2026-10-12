{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Specs for composition-level behavior. Real widget trees run through
-- the production host ('Reflex.Vty.Test.AppHarness.withApp'). Each test
-- asserts the observable widget state and the rendered output ('Grid').
module Reflex.Vty.CompositionSpec (spec) where

import Control.Monad (forM_, void)
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Control.Monad.NodeId (MonadNodeId)
import Data.Text.Zipper (fromText, value)
import Reflex.Vty.Host
import Reflex.Vty.Style (withForeground)
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..), attrAt)
import Reflex.Vty.Theme
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Box
import Reflex.Vty.Widget.Input.Mouse (MouseDown (..), mouseDown)
import Reflex.Vty.Widget.Input.Text
import Reflex.Vty.Widget.Layout
import Reflex.Vty.Widget.Scroll
import Reflex.Vty.Widget.Split
import Reflex.Vty.Widget.Text (fill)

spec :: Spec
spec = describe "Reflex.Vty composition" $ do
  nestedPaneSpec
  focusIsolationSpec
  simultaneousModificationSpec
  nestedAllocationSpec
  scrollLayoutInputSpec
  splitDragSpec
  captureReplacementSpec
  contextIsolationSpec
  smallBoundarySpec

----------------------------------------------------------------------------
-- Grid helpers
----------------------------------------------------------------------------

gridRowText :: Int -> Grid -> String
gridRowText y g
  | y < 0 || y >= gridHeight g = ""
  | otherwise = T.unpack . T.concat . map renderSpanText $ gridRows g !! y

-- | 'gridRowText' with the render padding's trailing spaces stripped.
gridRowTrimmed :: Int -> Grid -> String
gridRowTrimmed y g = reverse (dropWhile (== ' ') (reverse (gridRowText y g)))

-- | The subrectangle of the grid as strings, @x0 y0 w h@.
gridSubLines :: Int -> Int -> Int -> Int -> Grid -> [String]
gridSubLines x0 y0 w h g =
  [take w (drop x0 (gridRowText y g)) | y <- [y0 .. y0 + h - 1]]

countChar :: Char -> Grid -> Int
countChar c g = length (filter (== c) (concat [gridRowText y g | y <- [0 .. gridHeight g - 1]]))

----------------------------------------------------------------------------
-- 1. Nested pane equivalence
----------------------------------------------------------------------------

innerField
  :: ( Reflex t
     , MonadHold t m
     , MonadFix m
     , HasInput t m
     , HasFocusReader t m
     , HasTheme t m
     , HasDisplayRegion t m
     , HasImageWriter t m
     )
  => m (Dynamic t (Text, (Int, Int)))
innerField = do
  md <- mouseDown V.BLeft
  clicks <- holdDyn (99, 99) (_mouseDown_coordinates <$> md)
  ti <- textInput def {_textInputConfig_initialValue = fromText "abcdef"}
  pure (zipDyn (_textInput_value ti) clicks)

-- | A field inside pane (3,2,12,5) which itself contains pane (1,1,8,3):
-- the field's global origin is (4,3).
nestedPaneApp :: TestApp () (Text, (Int, Int))
nestedPaneApp _ = do
  d <-
    pane (pure (Region 3 2 12 5)) (pure True) $
      pane (pure (Region 1 1 8 3)) (pure True) innerField
  pure (d, never)

-- | The same field placed directly at the summed origin.
flatPaneApp :: TestApp () (Text, (Int, Int))
flatPaneApp _ = do
  d <- pane (pure (Region 4 3 8 3)) (pure True) innerField
  pure (d, never)

nestedPaneSpec :: Spec
nestedPaneSpec = describe "nested pane equivalence" $ do
  it "translates clicks through both panes like a single summed origin" $ do
    nested <- withApp defaultVtyAppConfig (20, 8) nestedPaneApp $ \driver -> do
      f1 <- stepInput driver (V.EvMouseDown 5 4 V.BLeft []) -- global (5,4) -> local (1,1)
      frameObservation f1 `shouldBe` ("abcdef", (1, 1))
      _ <- stepInput driver (V.EvMouseDown 6 3 V.BLeft []) -- -> local (2,0): cursor after "ab"
      f3 <- stepInput driver (V.EvKey (V.KChar 'X') [])
      -- Clicking at column 2 puts the zipper cursor before 'c'. The
      -- keystroke inserts there; it does not overwrite.
      frameObservation f3 `shouldBe` ("abXcdef", (2, 0))
      sub <- gridSubLines 4 3 8 3 . frameGrid <$> lastFrame driver
      stopApp driver
      pure (frameObservation f3, sub)
    flat <- withApp defaultVtyAppConfig (20, 8) flatPaneApp $ \driver -> do
      _ <- stepInput driver (V.EvMouseDown 5 4 V.BLeft [])
      _ <- stepInput driver (V.EvMouseDown 6 3 V.BLeft [])
      f3 <- stepInput driver (V.EvKey (V.KChar 'X') [])
      sub <- gridSubLines 4 3 8 3 . frameGrid <$> lastFrame driver
      stopApp driver
      pure (frameObservation f3, sub)
    nested `shouldBe` flat
    -- The shared content subrectangle really contains the typed text
    -- (compare content, not unrelated padding).
    case snd nested of
      (top : _) -> take 6 top `shouldBe` "abXcde"
      [] -> expectationFailure "expected at least one content row"

lastFrame :: AppDriver c o -> IO (Frame o)
lastFrame driver = do
  fs <- driverFrames driver
  case reverse fs of
    (f : _) -> pure f
    [] -> fail "expected at least one frame"

----------------------------------------------------------------------------
-- 2. Focus isolation
----------------------------------------------------------------------------

twoInputApp :: TestApp () (Text, Text)
twoInputApp _ = do
  (t1, t2) <- initManager_ $ do
    tabNavigation
    row $ do
      a <- textInputTile (textInput def) (pure 9)
      b <- textInputTile (textInput def) (pure 9)
      pure (a, b)
  pure (zipDyn (_textInput_value t1) (_textInput_value t2), never)

focusIsolationSpec :: Spec
focusIsolationSpec = describe "focus isolation in a managed row" $ do
  it "delivers typing, paste, and BackTab only to the focused tile" $
    withApp defaultVtyAppConfig (20, 3) twoInputApp $ \driver -> do
      f0 <- stepInput driver (V.EvKey (V.KChar 'z') [])
      frameObservation f0 `shouldBe` ("", "") -- nothing focused yet
      _ <- stepInput driver (V.EvKey (V.KChar '\t') []) -- select first tile
      f1 <- stepInput driver (V.EvKey (V.KChar 'a') [])
      frameObservation f1 `shouldBe` ("a", "")
      _ <- stepInput driver (V.EvKey (V.KChar '\t') []) -- select second tile
      f2 <- stepInput driver (V.EvPaste (TE.encodeUtf8 "界"))
      frameObservation f2 `shouldBe` ("a", "界")
      _ <- stepInput driver (V.EvKey V.KBackTab []) -- back to first tile
      f4 <- stepInput driver (V.EvKey (V.KChar 'b') [])
      frameObservation f4 `shouldBe` ("ab", "界")
      take 2 (gridRowText 0 (frameGrid f4)) `shouldBe` "ab"
      stopApp driver

  it "a click moves the cursor and does not duplicate typed input" $
    withApp defaultVtyAppConfig (20, 3) twoInputApp $ \driver -> do
      _ <- stepInput driver (V.EvKey (V.KChar '\t') [])
      _ <- stepInput driver (V.EvKey (V.KChar 'a') [])
      _ <- stepInput driver (V.EvKey (V.KChar 'b') [])
      _ <- stepInput driver (V.EvMouseDown 0 0 V.BLeft []) -- cursor to col 0
      f <- stepInput driver (V.EvKey (V.KChar 'c') [])
      frameObservation f `shouldBe` ("cab", "")
      take 3 (gridRowText 0 (frameGrid f)) `shouldBe` "cab"
      stopApp driver

----------------------------------------------------------------------------
-- 3. Simultaneous UI input and caller modification
----------------------------------------------------------------------------

data SimCmd = SimBoth | SimModifyOnly
  deriving (Eq)

simApp :: TestApp SimCmd (Text, Maybe Text, Int)
simApp cmd = do
  let syntheticY = V.EvKey (V.KChar 'y') [] <$ fforMaybe cmd (\c -> if c == SimBoth then Just () else Nothing)
      -- Replace the whole zipper. The final value is the modify result,
      -- even when a UI keystroke occurs in the same frame.
      modE = fforMaybe cmd $ \case
        SimBoth -> Just (const (fromText "SET"))
        SimModifyOnly -> Just (const (fromText "MOD"))
  ti <-
    localInput (\e -> leftmost [e, syntheticY]) $
      textInput def {_textInputConfig_modify = modE}
  n <- count (_textInput_userInput ti)
  lastUI <- holdDyn Nothing (Just . value <$> _textInput_userInput ti)
  pure ((,,) <$> _textInput_value ti <*> lastUI <*> n, never)

simultaneousModificationSpec :: Spec
simultaneousModificationSpec =
  describe "simultaneous UI input and _textInputConfig_modify" $
    it "modify sets the final value; userInput contains the UI-only result" $
      withApp defaultVtyAppConfig (10, 3) simApp $ \driver -> do
        f0 <- lastFrame driver
        frameObservation f0 `shouldBe` ("", Nothing, 0)
        f1 <- stepCommand driver SimBoth
        frameObservation f1 `shouldBe` ("SET", Just "y", 1)
        take 3 (gridRowText 0 (frameGrid f1)) `shouldBe` "SET"
        f2 <- stepCommand driver SimModifyOnly
        frameObservation f2 `shouldBe` ("MOD", Just "y", 1) -- no new userInput
        take 3 (gridRowText 0 (frameGrid f2)) `shouldBe` "MOD"
        stopApp driver

----------------------------------------------------------------------------
-- 4. Nested allocation
----------------------------------------------------------------------------

data AllocCmd = AllocReorder | AllocDropA

-- | Paint a one-character label at the child region's origin.
label
  :: (Reflex t, Monad m, HasImageWriter t m)
  => Char
  -> m ()
label c = tellImages (pure [V.string V.defAttr [c]])

-- | Run a 'Dynamic' of child widgets, replacing the child on updates.
switchWidget :: (Reflex t, Adjustable t m, MonadSample t m) => Dynamic t (m ()) -> m ()
switchWidget d = do
  d0 <- sample (current d)
  _ <- runWithReplace d0 (updated d)
  pure ()

allocApp :: TestApp AllocCmd [Char]
allocApp cmd = do
  kidsD <- holdDyn ['A', 'B', 'C'] $
    fforMaybe cmd $ \case
      AllocReorder -> Just ['C', 'A', 'B']
      AllocDropA -> Just ['B', 'C']
  initManager_ . col . switchWidget . ffor kidsD $ \ls -> do
    -- Children get regions in list order. Each 'C' takes one full-width
    -- flex slot. Each run of non-'C' children forms one fixed-3 row:
    -- A is fixed 5 wide, and B takes the rest.
    let place [] = pure ()
        place ('C' : rest) = grout flex (label 'C') >> place rest
        place rest = do
          let (rowKids, rest') = span (/= 'C') rest
          axis (pure Orientation_Row) (fixed (pure 3)) $
            forM_ rowKids $ \c ->
              grout (if c == 'A' then fixed (pure 5) else flex) (label c)
          place rest'
    place ls
  pure (kidsD, never)

nestedAllocationSpec :: Spec
nestedAllocationSpec = describe "nested allocation" $ do
  it "allocates fixed 5, flex, and outer flex in order" $
    withApp defaultVtyAppConfig (20, 10) allocApp $ \driver -> do
      f0 <- lastFrame driver
      frameObservation f0 `shouldBe` ['A', 'B', 'C']
      -- A fixed 5x3 at (0,0), B flex 15x3 at (5,0), C flex 20x7 at (0,3)
      take 1 (gridRowText 0 (frameGrid f0)) `shouldBe` "A"
      take 1 (drop 5 (gridRowText 0 (frameGrid f0))) `shouldBe` "B"
      take 1 (gridRowText 3 (frameGrid f0)) `shouldBe` "C"
      stopApp driver

  it "reorders, resizes, and removes a held child" $
    withApp defaultVtyAppConfig (20, 10) allocApp $ \driver -> do
      f1 <- stepCommand driver AllocReorder
      frameObservation f1 `shouldBe` ['C', 'A', 'B']
      take 1 (gridRowText 0 (frameGrid f1)) `shouldBe` "C" -- C flex on top, rows 0-6
      take 1 (gridRowText 7 (frameGrid f1)) `shouldBe` "A" -- fixed row now at rows 7-9
      take 1 (drop 5 (gridRowText 7 (frameGrid f1))) `shouldBe` "B"
      f2 <- resizeApp driver (10, 10)
      take 1 (gridRowText 0 (frameGrid f2)) `shouldBe` "C"
      take 1 (gridRowText 7 (frameGrid f2)) `shouldBe` "A"
      take 1 (drop 5 (gridRowText 7 (frameGrid f2))) `shouldBe` "B"
      f3 <- stepCommand driver AllocDropA
      frameObservation f3 `shouldBe` ['B', 'C']
      take 1 (gridRowText 0 (frameGrid f3)) `shouldBe` "B" -- B flexes to full width
      take 1 (gridRowText 3 (frameGrid f3)) `shouldBe` "C"
      stopApp driver

----------------------------------------------------------------------------
-- 5. Scroll / layout / input
----------------------------------------------------------------------------

data ScrollCmd = ScrollAppend | ScrollFollow
  deriving (Eq)

scrollApp :: TestApp ScrollCmd ((Int, Int), Int)
scrollApp cmd = do
  appends <- count (fforMaybe cmd (\c -> if c == ScrollAppend then Just () else Nothing))
  let rowsD = (8 +) <$> appends
  followD <-
    holdDyn
      (Just ScrollToBottom_Maintain)
      (Just ScrollToBottom_Always <$ fforMaybe cmd (\c -> if c == ScrollFollow then Just () else Nothing))
  let mkContent = do
        md <- mouseDown V.BLeft
        clicks <- holdDyn (99, 99) (_mouseDown_coordinates <$> md)
        tellImages $
          ffor (current rowsD) $ \n ->
            [V.vertCat [V.string V.defAttr (show i) | i <- [1 .. n]]]
        pure (void (updated rowsD), clicks)
  (_, clicks) <-
    pane (pure (Region 0 0 12 4)) (pure True) $
      scrollable
        def
          { _scrollableConfig_scrollToBottom = current followD
          , _scrollableConfig_scrollbarVisibility = ScrollbarHidden
          }
        mkContent
  pure (zipDyn clicks rowsD, never)

scrollBoxApp :: TestApp () ((Int, Int), Int)
scrollBoxApp _ = do
  let mkContent = do
        md <- mouseDown V.BLeft
        clicks <- holdDyn (99, 99) (_mouseDown_coordinates <$> md)
        tellImages (pure [V.vertCat [V.string V.defAttr (show i) | i <- [1 .. 8 :: Int]]])
        pure (never, clicks)
  (_, clicks) <-
    initManager_ . tile flex . boxStatic singleBoxStyle $
      scrollable
        def {_scrollableConfig_scrollbarVisibility = ScrollbarHidden}
        mkContent
  pure (zipDyn clicks (pure (8 :: Int)), never)

scrollLayoutInputSpec :: Spec
scrollLayoutInputSpec = describe "scrollable composition" $ do
  it "scrolls, translates clicks to content coordinates, and follows appends" $
    withApp defaultVtyAppConfig (12, 4) scrollApp $ \driver -> do
      f0 <- lastFrame driver
      mapM_
        (\(y, e) -> gridRowTrimmed y (frameGrid f0) `shouldBe` e)
        [(0, "1"), (1, "2"), (2, "3"), (3, "4")]
      _ <- stepInput driver (V.EvKey V.KDown [])
      f2 <- stepInput driver (V.EvKey V.KDown [])
      mapM_
        (\(y, e) -> gridRowTrimmed y (frameGrid f2) `shouldBe` e)
        [(0, "3"), (1, "4"), (2, "5"), (3, "6")]
      fc <- stepInput driver (V.EvMouseDown 0 1 V.BLeft [])
      frameObservation fc `shouldBe` ((0, 3), 8) -- viewport (0,1) -> content y=3
      fA <- stepCommand driver ScrollAppend
      gridRowTrimmed 0 (frameGrid fA) `shouldBe` "3" -- Maintain keeps view
      frameObservation fA `shouldBe` ((0, 3), 9)
      _ <- stepInput driver (V.EvKey V.KDown [])
      _ <- stepInput driver (V.EvKey V.KDown [])
      fB <- stepInput driver (V.EvKey V.KDown []) -- now at the bottom
      gridRowTrimmed 0 (frameGrid fB) `shouldBe` "6"
      _ <- stepCommand driver ScrollFollow
      fF <- stepCommand driver ScrollAppend -- at bottom: follow the append
      gridRowTrimmed 0 (frameGrid fF) `shouldBe` "7"
      gridRowTrimmed 3 (frameGrid fF) `shouldBe` "10"
      stopApp driver

  it "inside a tile and a box: one-cell inset, no double translation" $
    withApp defaultVtyAppConfig (12, 8) scrollBoxApp $ \driver -> do
      _ <- stepInput driver (V.EvMouseDown 1 1 V.BLeft [])
      _ <- stepInput driver (V.EvMouseUp 1 1 (Just V.BLeft))
      _ <- stepInput driver (V.EvKey V.KDown [])
      _ <- stepInput driver (V.EvKey V.KDown [])
      f <- stepInput driver (V.EvMouseDown 1 2 V.BLeft []) -- box inset (1,1), scroll offset 2
      take 1 (drop 1 (gridRowText 1 (frameGrid f))) `shouldBe` "3"
      frameObservation f `shouldBe` ((0, 3), 8)
      stopApp driver

----------------------------------------------------------------------------
-- 6. Drag and resize of splitVDrag
----------------------------------------------------------------------------

splitApp :: TestApp () ()
splitApp _ = do
  void $ splitVDrag (fill (pure '-')) (fill (pure 'A')) (fill (pure 'B'))
  pure (pure (), never)

-- | Drag from row @fromY@ to row @toY@ and return the final frame.
dragDivider :: AppDriver () () -> Int -> Int -> IO (Frame ())
dragDivider driver fromY toY = do
  _ <- stepInput driver (V.EvMouseDown 3 fromY V.BLeft [])
  _ <- stepInput driver (V.EvMouseDown 3 toY V.BLeft [])
  stepInput driver (V.EvMouseUp 3 toY (Just V.BLeft))

splitDragSpec :: Spec
splitDragSpec = describe "splitVDrag drag and resize" $ do
  it "starts at half height, drags, and re-drags after resize" $
    withApp defaultVtyAppConfig (20, 10) splitApp $ \driver -> do
      f0 <- lastFrame driver
      gridRowText 5 (frameGrid f0) `shouldBe` replicate 20 '-' -- divider row 5
      gridRowText 0 (frameGrid f0) `shouldBe` replicate 20 'A' -- A rows 0..4
      gridRowText 9 (frameGrid f0) `shouldBe` replicate 20 'B' -- B rows 6..9
      _ <- stepInput driver (V.EvMouseDown 3 5 V.BLeft [])
      moving <- stepInput driver (V.EvMouseDown 3 7 V.BLeft [])
      gridRowText 7 (frameGrid moving) `shouldBe` replicate 20 '-'
      f1 <- stepInput driver (V.EvMouseUp 3 7 (Just V.BLeft))
      gridRowText 7 (frameGrid f1) `shouldBe` replicate 20 '-'
      gridRowText 6 (frameGrid f1) `shouldBe` replicate 20 'A'
      gridRowText 8 (frameGrid f1) `shouldBe` replicate 20 'B'
      f2 <- resizeApp driver (20, 20) -- fraction 0.7 keeps divider at row 14
      gridRowText 14 (frameGrid f2) `shouldBe` replicate 20 '-'
      gridRowText 13 (frameGrid f2) `shouldBe` replicate 20 'A'
      gridRowText 15 (frameGrid f2) `shouldBe` replicate 20 'B'
      f3 <- dragDivider driver 14 3 -- repeated drag
      gridRowText 3 (frameGrid f3) `shouldBe` replicate 20 '-'
      gridRowText 2 (frameGrid f3) `shouldBe` replicate 20 'A'
      gridRowText 4 (frameGrid f3) `shouldBe` replicate 20 'B'
      stopApp driver

  it "clamps out-of-bounds drags without negative extents" $
    withApp defaultVtyAppConfig (20, 10) splitApp $ \driver -> do
      f1 <- dragDivider driver 5 100 -- far below the viewport
      gridRowText 9 (frameGrid f1) `shouldBe` replicate 20 '-' -- clamped to last row
      gridRowText 8 (frameGrid f1) `shouldBe` replicate 20 'A' -- B has zero height
      f2 <- dragDivider driver 9 (-1) -- above the viewport
      gridRowText 0 (frameGrid f2) `shouldBe` replicate 20 '-' -- clamped to row 0
      gridRowText 1 (frameGrid f2) `shouldBe` replicate 20 'B' -- A has zero height
      gridHeight (frameGrid f2) `shouldBe` 10 -- nothing shrank negatively
      stopApp driver

  it "works at tiny heights" $ do
    withApp defaultVtyAppConfig (20, 1) splitApp $ \driver -> do
      f <- lastFrame driver
      gridRowText 0 (frameGrid f) `shouldBe` replicate 20 '-'
      stopApp driver
    withApp defaultVtyAppConfig (20, 2) splitApp $ \driver -> do
      f <- lastFrame driver
      gridRowText 0 (frameGrid f) `shouldBe` replicate 20 'A'
      gridRowText 1 (frameGrid f) `shouldBe` replicate 20 '-'
      stopApp driver

----------------------------------------------------------------------------
-- 7. Capture and replacement
----------------------------------------------------------------------------

data CapCmd = CapEmit | CapRemove
  deriving (Eq)

data CapMode = CapHeld | CapShown | CapGone
  deriving (Eq)

-- | A managed child: paints a marker plus its own 'x'-press count,
-- requests a terminal cursor, and owns a focus tile.
capChild
  :: ( Reflex t
     , MonadHold t m
     , MonadFix m
     , MonadNodeId m
     , MonadSample t m
     , PostBuild t m
     , HasInput t m
     , HasImageWriter t m
     , HasDisplayRegion t m
     , HasFocusReader t m
     , HasCursor t m
     , HasFocus t m
     , HasLayout t m
     , HasTheme t m
     )
  => m ()
capChild = do
  _ <- tile' flex $ do
    xE <- key (V.KChar 'x')
    n <- count xE
    tellImages $ ffor (current n) $ \k -> [V.string V.defAttr ('M' : show k)]
    setCursor (pure (CursorState CursorVisible CursorStyleDefault (0, 0)))
  pure ()

capApp :: TestApp CapCmd Bool
capApp cmd = do
  modeD <-
    holdDyn CapHeld $
      leftmost
        [ CapShown <$ fforMaybe cmd (\c -> if c == CapEmit then Just () else Nothing)
        , CapGone <$ fforMaybe cmd (\c -> if c == CapRemove then Just () else Nothing)
        ]
  removed <- holdUniqDyn ((== CapGone) <$> modeD)
  initManager_ $ do
    tabNavigation
    switchWidget $ ffor removed $ \gone ->
      if gone
        then blank
        else do
          (_, images) <- captureImages capChild
          tellImages $ (\mode imgs -> if mode == CapShown then imgs else []) <$> current modeD <*> images
  pure ((== CapGone) <$> modeD, never)

captureReplacementSpec :: Spec
captureReplacementSpec =
  describe "capture and replacement of a managed child" $
    it "keeps the image, re-emits once, and leaves no old image or stale handler" $
      withApp defaultVtyAppConfig (10, 4) capApp $ \driver -> do
        f0 <- lastFrame driver
        countChar 'M' (frameGrid f0) `shouldBe` 0 -- marker captured, not painted
        frameCursor f0 `shouldBe` V.AbsoluteCursor 0 0 -- cursor request still applies
        _ <- stepInput driver (V.EvKey (V.KChar '\t') [])
        f1 <- stepInput driver (V.EvKey (V.KChar 'x') [])
        countChar 'M' (frameGrid f1) `shouldBe` 0 -- still held
        f2 <- stepCommand driver CapEmit -- re-emit exactly once
        countChar 'M' (frameGrid f2) `shouldBe` 1
        -- Re-emitting the captured images preserves the child's state.
        take 2 (gridRowText 0 (frameGrid f2)) `shouldBe` "M1"
        f3 <- stepInput driver (V.EvKey (V.KChar 'x') [])
        countChar 'M' (frameGrid f3) `shouldBe` 1
        take 2 (gridRowText 0 (frameGrid f3)) `shouldBe` "M2"
        f4 <- stepCommand driver CapRemove -- replace/remove the child
        countChar 'M' (frameGrid f4) `shouldBe` 0 -- no old image
        f5 <- stepInput driver (V.EvKey (V.KChar 'x') []) -- no stale handler
        countChar 'M' (frameGrid f5) `shouldBe` 0
        frameGrid f5 `shouldBe` frameGrid f4 -- nothing changed at all
        frameObservation f5 `shouldBe` True
        stopApp driver

----------------------------------------------------------------------------
-- 8. Context isolation (themes, cursors)
----------------------------------------------------------------------------

data ThemeCmd = ThemeFlip

flippedTheme :: Theme
flippedTheme =
  defTheme
    { -- textInput paints its text with the ambient default style
      -- ('themeAttr') and its cursor span with '_theme_textInputCursor'.
      _theme_default = withForeground (V.rgbColor 200 0 0) def
    , _theme_textInputCursor = withForeground (V.rgbColor 200 0 0) def
    }

themeApp :: TestApp ThemeCmd (Text, Text)
themeApp cmd = do
  t1 <- holdDyn defTheme (flippedTheme <$ cmd)
  ti1 <-
    pane (pure (Region 0 0 20 4)) (pure True) $
      localTheme (const (current t1)) $
        textInput def {_textInputConfig_initialValue = fromText "a"}
  ti2 <-
    pane (pure (Region 0 5 20 4)) (pure False) $
      localTheme (const (pure defTheme)) $
        textInput def {_textInputConfig_initialValue = fromText "b"}
  pure (zipDyn (_textInput_value ti1) (_textInput_value ti2), never)

wideCursorApp :: TestApp () ()
wideCursorApp _ = do
  -- The cursor widget sits in a contained inner pane. The terminal
  -- cursor must add the viewport origins of both panes.
  pane (pure (Region 2 2 10 3)) (pure True) $
    pane (pure (Region 1 1 8 3)) (pure True) $ do
      tellImages (pure [V.string V.defAttr "a\30028|"])
      setCursor (pure (CursorState CursorVisible CursorStyleDefault (3, 0)))
  pure (pure (), never)

contextIsolationSpec :: Spec
contextIsolationSpec = describe "context isolation" $ do
  it "a local theme change restyles only its own subtree, across a resize" $
    withApp defaultVtyAppConfig (20, 10) themeApp $ \driver -> do
      f0 <- lastFrame driver
      frameObservation f0 `shouldBe` ("a", "b")
      let g0 = frameGrid f0
      attr0 <- pure (attrAt 0 0 g0)
      f1 <- stepCommand driver ThemeFlip
      let g1 = frameGrid f1
      attrAt 0 0 g1 `shouldNotBe` attr0 -- text attribute changed
      attrAt 1 0 g1 `shouldNotBe` attrAt 1 0 g0 -- image cursor changed
      attrAt 0 5 g1 `shouldBe` attrAt 0 5 g0 -- second box untouched
      frameObservation f1 `shouldBe` ("a", "b")
      f2 <- resizeApp driver (30, 10)
      let g2 = frameGrid f2
      attrAt 0 0 g2 `shouldBe` attrAt 0 0 g1 -- unchanged by the resize
      attrAt 1 0 g2 `shouldBe` attrAt 1 0 g1
      attrAt 0 5 g2 `shouldBe` attrAt 0 5 g1
      f3 <- stepInput driver (V.EvKey (V.KChar 'z') []) -- still focused, values intact
      frameObservation f3 `shouldBe` ("az", "b")
      stopApp driver

  it "setCursor over a wide glyph reports cell coordinates from the pane origin" $
    withApp defaultVtyAppConfig (16, 5) wideCursorApp $ \driver -> do
      f0 <- lastFrame driver
      -- Inner pane's composed viewport origin is (2,2)+(1,1) = (3,3);
      -- local (3,0) is 3 *cells* from that origin, so the cell is '|'
      -- (character indexing would put it one cell further right).
      frameCursor f0 `shouldBe` V.AbsoluteCursor 6 3
      take 3 (drop 3 (gridRowText 3 (frameGrid f0))) `shouldBe` "a\30028|"
      stopApp driver

----------------------------------------------------------------------------
-- Small boundaries
----------------------------------------------------------------------------

narrowInputApp :: TestApp () Text
narrowInputApp _ = do
  ti <- textInput def
  pure (_textInput_value ti, never)

smallBoundarySpec :: Spec
smallBoundarySpec = describe "small boundaries" $ do
  it "wraps typed text and follows the cursor in a one-column viewport" $
    withApp defaultVtyAppConfig (1, 2) narrowInputApp $ \driver -> do
      _ <- stepInput driver (V.EvKey (V.KChar 'a') [])
      f <- stepInput driver (V.EvKey (V.KChar 'b') [])
      frameObservation f `shouldBe` "ab"
      -- The insertion cursor wraps to a third row; a two-row viewport
      -- follows it, showing the last character and its blank cursor row.
      gridRowText 0 (frameGrid f) `shouldBe` "b"
      gridRowText 1 (frameGrid f) `shouldBe` " "
      top <- stepInput driver (V.EvKey V.KHome [])
      gridRowText 0 (frameGrid top) `shouldBe` "a"
      gridRowText 1 (frameGrid top) `shouldBe` "b"
      stopApp driver

  it "a zero-height terminal produces an empty but well-formed grid" $
    withApp defaultVtyAppConfig (20, 0) splitApp $ \driver -> do
      f <- lastFrame driver
      gridHeight (frameGrid f) `shouldBe` 0
      stopApp driver
