{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Acceptance coverage for the production Tier A demo widget.
--
-- Everything here drives 'Example.TierA.demoWidget' through the
-- deterministic production-stack harness. The demo's returned shutdown
-- event feeds a counter that is exposed as the observation, so shutdown
-- routing can be asserted without ever stopping the harness host (the
-- harness's own shutdown channel stays 'never').
--
-- Assertions name the content and its absolute screen coordinates
-- rather than whole-screen goldens: the baseline is 80x24 with status
-- row 0, seven help rows (1-7), grid rows 8-20, paginator row 21, and
-- hint rows 22-23.
module Example.TierASpec (spec) where

import Control.Monad (foldM)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Example.TierA (demoWidget)
import Reflex.Vty (defaultVtyAppConfig)
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (gridRows, renderSpanText)

-- | Runs the production demo in the recording host while counting its
-- shutdown requests.
wrapDemo :: TestApp () Int
wrapDemo _ = do
  shutdown <- demoWidget
  shutdownCount <- foldDyn (\() n -> n + 1) (0 :: Int) shutdown
  pure (shutdownCount, never)

type Demo = AppDriver () Int

lastFrame :: Demo -> IO (Frame Int)
lastFrame d = last <$> driverFrames d

-- * Screen helpers

lineAt :: Int -> Frame Int -> String
lineAt y f =
  case drop y (gridRows (frameGrid f)) of
    (spans : _) -> T.unpack (T.concat (map renderSpanText spans))
    [] -> ""

linesOf :: Frame Int -> [String]
linesOf = map (T.unpack . T.concat . map renderSpanText) . gridRows . frameGrid

numbered :: Frame Int -> String
numbered f =
  unlines
    [ show y ++ ": " ++ line
    | (y, line) <- zip [0 :: Int ..] (linesOf f)
    ]

findColumn :: String -> String -> Maybe Int
findColumn needle hay = go 0 hay
  where
    go n s
      | needle `isPrefixOf` s = Just n
      | null s = Nothing
      | otherwise = go (n + 1) (drop 1 s)

findText :: String -> Frame Int -> Maybe (Int, Int)
findText needle f =
  case
    [ (y, c)
    | (y, line) <- zip [0 ..] (linesOf f)
    , Just c <- [findColumn needle line]
    ] of
    (p : _) -> Just p
    [] -> Nothing

expectAt :: String -> Int -> Int -> Frame Int -> Expectation
expectAt needle y x f =
  case findColumn needle (lineAt y f) of
    Just c -> c `shouldBe` x
    Nothing ->
      expectationFailure $
        "missing " ++ show needle ++ " on row " ++ show y ++ "\n" ++ numbered f

expectOnRow :: String -> Int -> Frame Int -> Expectation
expectOnRow needle y f =
  case findColumn needle (lineAt y f) of
    Just _ -> pure ()
    Nothing ->
      expectationFailure $
        "missing " ++ show needle ++ " on row " ++ show y ++ "\n" ++ numbered f

expectAbsent :: String -> Frame Int -> Expectation
expectAbsent needle f =
  case findText needle f of
    Nothing -> pure ()
    Just (y, x) ->
      expectationFailure $
        "unexpected " ++ show needle ++ " at " ++ show (y, x) ++ "\n" ++ numbered f

-- * Input helpers

charKey :: Char -> V.Event
charKey c = V.EvKey (V.KChar c) []

funKey :: Int -> V.Event
funKey n = V.EvKey (V.KFun n) []

tabKey, backTabKey, escKey :: V.Event
tabKey = charKey '\t'
backTabKey = V.EvKey V.KBackTab []
escKey = V.EvKey V.KEsc []

ctrlLeft, ctrlRight, ctrlC :: V.Event
ctrlLeft = V.EvKey V.KLeft [V.MCtrl]
ctrlRight = V.EvKey V.KRight [V.MCtrl]
ctrlC = V.EvKey (V.KChar 'c') [V.MCtrl]

-- | Type a string one key at a time, returning the final fenced frame.
typeString :: Demo -> String -> IO (Frame Int)
typeString d s = do
  f0 <- lastFrame d
  foldM (\_ e -> stepInput d e) f0 (map charKey s)

-- | The dialog occupies a 52x12 fitted panel centered in 80x24, so its
-- top-left is (14,6), its title is centered on the top edge, and its
-- body starts at (15,7).
expectDialogAt80x24 :: Frame Int -> Expectation
expectDialogAt80x24 f = do
  expectAt "Tier A dialog" 6 34 f
  expectAt "First:" 7 15 f
  expectAt "Second:" 8 15 f
  expectAt "p: open popover" 9 15 f
  expectAt "Tab/Shift+Tab: switch field" 10 15 f
  expectAt "Escape: close dialog" 11 15 f

spec :: Spec
spec = describe "Example.TierA" $ do
  describe "background controls, grid, and paginator" $ do
    it "dispatches remapped/enabled actions and preserves grid state across layout changes" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        f0 <- lastFrame d
        frameObservation f0 `shouldBe` 0
        -- Status row and generated two-column help from the same live map.
        expectAt "count 0" 0 0 f0
        expectOnRow "increment key 'x' enabled" 0 f0
        expectAt "x  increment counter" 1 0 f0
        expectAt "r  remap increment x/i" 2 0 f0
        expectAt "m  open dialog" 5 41 f0
        expectAt "q  quit" 6 41 f0
        -- Five dots on the paginator row, never on the status row.
        expectAt "● ○ ○ ○ ○" 21 0 f0
        findColumn "●" (lineAt 0 f0) `shouldBe` Nothing
        -- Six cells in three columns, gap and padding 1.
        expectAt "A" 9 1 f0
        expectAt "B" 9 28 f0
        expectAt "C" 9 54 f0
        expectAt "D" 15 1 f0
        expectAt "E" 15 28 f0
        expectAt "F" 15 54 f0

        -- 'x' is consumed by the root map: count 2, no typing in cell A.
        _ <- stepInput d (charKey 'x')
        f2 <- stepInput d (charKey 'x')
        frameObservation f2 `shouldBe` 0
        expectAt "count 2" 0 0 f2
        expectAt "A" 9 1 f2

        -- Remap: the live map, status, and help all move to 'i'.
        fR <- stepInput d (charKey 'r')
        expectOnRow "increment key 'i' enabled" 0 fR
        expectAt "i  increment counter" 1 0 fR
        -- 'x' is now unbound and reaches the focused field.
        fX <- stepInput d (charKey 'x')
        expectAt "count 2" 0 0 fX
        expectAt "Ax" 9 1 fX
        fI <- stepInput d (charKey 'i')
        expectAt "count 3" 0 0 fI

        -- Disabling removes increment from dispatch and generated help;
        -- the key then passes through to the field.
        fD1 <- stepInput d (charKey 'd')
        expectOnRow "increment key 'i' DISABLED" 0 fD1
        expectAbsent "increment counter" fD1
        expectAt "Ax" 9 1 fD1
        fPass <- stepInput d (charKey 'i')
        expectAt "Axi" 9 1 fPass
        expectAt "count 3" 0 0 fPass
        expectAbsent "increment counter" fPass
        -- Re-enabling restores dispatch and help.
        fD2 <- stepInput d (charKey 'd')
        expectOnRow "increment key 'i' enabled" 0 fD2
        expectOnRow "increment counter" 1 fD2
        f4 <- stepInput d (charKey 'i')
        expectAt "count 4" 0 0 f4

        -- Paste into the focused cell, then two-column regrouping.
        fPaste <- stepInput d (V.EvPaste "saved")
        expectAt "Axisaved" 9 1 fPaste
        f2col <- stepInput d (funKey 6)
        expectAt "Axisaved" 9 1 f2col
        expectAt "B" 9 41 f2col
        expectAt "C" 13 1 f2col
        expectAt "D" 13 41 f2col
        expectAt "E" 17 1 f2col
        expectAt "F" 17 41 f2col
        fZ <- stepInput d (charKey 'Z')
        expectAt "AxisavedZ" 9 1 fZ

        -- Gap increase/decrease with saturation at zero.
        _ <- stepInput d (charKey '+')
        _ <- stepInput d (charKey '-')
        _ <- stepInput d (charKey '-')
        fGap0 <- stepInput d (charKey '-')
        expectAt "AxisavedZ" 9 1 fGap0
        expectAt "B" 9 40 fGap0
        fSaturated <- stepInput d (charKey '-')
        expectAt "AxisavedZ" 9 1 fSaturated
        expectAt "B" 9 40 fSaturated

        -- Padding removal shifts every cell to the padded-away origin.
        fPad0 <- stepInput d (funKey 7)
        expectAt "AxisavedZ" 8 0 fPad0
        expectAt "B" 8 40 fPad0
        expectAt "C" 13 0 fPad0
        expectAt "D" 13 40 fPad0

        -- Restore gap 1, then three columns, then padding 1.
        _ <- stepInput d (charKey '+')
        _ <- stepInput d (funKey 6)
        fRestored <- stepInput d (funKey 7)
        expectAt "AxisavedZ" 9 1 fRestored
        expectAt "B" 9 28 fRestored
        expectAt "C" 9 54 fRestored
        expectAt "D" 15 1 fRestored
        expectAt "E" 15 28 fRestored
        expectAt "F" 15 54 fRestored

        -- Tab moves focus to B; its text survives another regroup.
        _ <- stepInput d tabKey
        fB <- stepInput d (charKey 'K')
        expectAt "BK" 9 28 fB
        _ <- stepInput d (funKey 6)
        fBKZ <- stepInput d (charKey 'Z')
        expectAt "BKZ" 9 41 fBKZ
        expectAt "AxisavedZ" 9 1 fBKZ
        -- Shift+Tab walks back to A.
        _ <- stepInput d backTabKey
        fBack <- stepInput d (charKey 'Q')
        expectAt "AxisavedZQ" 9 1 fBack
        expectAt "BKZ" 9 41 fBack

        -- Pagination commands come from the same root action stream and
        -- never reach the grid fields.
        fNum <- stepInput d (funKey 8)
        expectAt "1/5" 21 0 fNum
        expectAbsent "●" fNum
        fNext <- stepInput d ctrlRight
        expectAt "2/5" 21 0 fNext
        _ <- stepInput d ctrlRight
        _ <- stepInput d ctrlRight
        fLast <- stepInput d ctrlRight
        expectAt "5/5" 21 0 fLast
        fSaturatedPage <- stepInput d ctrlRight
        expectAt "5/5" 21 0 fSaturatedPage
        fPrev <- stepInput d ctrlLeft
        expectAt "4/5" 21 0 fPrev
        fSmall <- stepInput d (funKey 9)
        expectAt "1/1" 21 0 fSmall
        fEmpty <- stepInput d (funKey 9)
        expectAt "0/0" 21 0 fEmpty
        fEmptyNav <- stepInput d ctrlRight
        expectAt "0/0" 21 0 fEmptyNav
        fFive <- stepInput d (funKey 9)
        expectAt "1/5" 21 0 fFive
        fDots <- stepInput d (funKey 8)
        expectAt "● ○ ○ ○ ○" 21 0 fDots
        expectAt "AxisavedZQ" 9 1 fDots
        expectAt "BKZ" 9 41 fDots
        expectAt "count 4" 0 0 fDots
        frameObservation fDots `shouldBe` 0
        stopApp d

  describe "dialog and nested popover" $ do
    it "isolates dialog input, persists both fields, and restores grid focus" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        f0 <- lastFrame d
        expectAt "count 0" 0 0 f0

        fOpen <- stepInput d (charKey 'm')
        expectDialogAt80x24 fOpen
        -- The full-viewport backdrop and centered panel hide the
        -- background entirely.
        expectAbsent "count 0" fOpen
        expectAbsent "increment counter" fOpen
        -- Both fields start empty: no opener or stray text before typing.
        take 6 (drop 23 (lineAt 7 fOpen)) `shouldBe` replicate 6 ' '
        take 6 (drop 23 (lineAt 8 fOpen)) `shouldBe` replicate 6 ' '
        frameObservation fOpen `shouldBe` 0

        -- Every root control types into the form while the dialog owns
        -- input; the opener is not replayed or inserted.
        _ <- typeString d "xrdq"
        fTyped <- stepInput d (charKey 'm')
        expectAt "xrdqm" 7 23 fTyped
        expectAt "Tier A dialog" 6 34 fTyped
        frameObservation fTyped `shouldBe` 0

        -- Tab reaches the second field only after the focus shift.
        _ <- stepInput d tabKey
        _ <- typeString d "tw"
        fTwo <- stepInput d (charKey 'o')
        expectAt "two" 8 23 fTwo
        expectAt "xrdqm" 7 23 fTwo

        -- Popup opens over the form; it covers the lower hints.
        fPop <- stepInput d (charKey 'p')
        expectAt "Nested popover" 10 15 fPop
        expectAt "Escape closes this popup" 11 15 fPop
        expectAt "Tier A dialog" 6 34 fPop
        expectAt "xrdqm" 7 23 fPop
        expectAt "two" 8 23 fPop
        expectAt "p: open popover" 9 15 fPop
        expectAbsent "Escape: close dialog" fPop
        expectAbsent "Tab/Shift+Tab: switch field" fPop

        -- Read-only popup: typing changes neither form nor background.
        _ <- typeString d "qx"
        fReadOnly <- stepInput d (charKey 'r')
        expectAt "xrdqm" 7 23 fReadOnly
        expectAt "two" 8 23 fReadOnly
        expectAt "Nested popover" 10 15 fReadOnly
        frameObservation fReadOnly `shouldBe` 0

        -- First Escape closes only the popup.
        fEscPopup <- stepInput d escKey
        expectAbsent "Nested popover" fEscPopup
        expectAbsent "Escape closes this popup" fEscPopup
        expectAt "Escape: close dialog" 11 15 fEscPopup
        expectAt "Tab/Shift+Tab: switch field" 10 15 fEscPopup
        expectAt "Tier A dialog" 6 34 fEscPopup

        -- Focus is back on the second field: Z appends there.
        fZ <- stepInput d (charKey 'Z')
        expectAt "twoZ" 8 23 fZ
        expectAt "xrdqm" 7 23 fZ

        -- Second Escape closes the dialog and restores background content.
        fClosed <- stepInput d escKey
        expectAbsent "Tier A dialog" fClosed
        expectAt "count 0" 0 0 fClosed
        expectOnRow "increment key 'x' enabled" 0 fClosed
        expectAt "A" 9 1 fClosed
        expectAt "● ○ ○ ○ ○" 21 0 fClosed
        frameObservation fClosed `shouldBe` 0

        -- Grid A still owns focus after the modal is gone.
        fFocus <- stepInput d (charKey 'Y')
        expectAt "AY" 9 1 fFocus
        expectAt "B" 9 28 fFocus

        -- Reopen: both form fields kept their values.
        fReopen <- stepInput d (charKey 'm')
        expectDialogAt80x24 fReopen
        expectAt "xrdqm" 7 23 fReopen
        expectAt "twoZ" 8 23 fReopen
        stopApp d

    it "defers popup outside-click dismissal to the release and does not replay it" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        _ <- stepInput d (charKey 'm')
        fTyped <- typeString d "ab"
        expectAt "ab" 7 23 fTyped
        fOpen <- stepInput d (charKey 'p')
        expectAt "Nested popover" 10 15 fOpen

        -- The down owns a gesture: the pending hide waits for its release.
        fDown <- stepInput d (V.EvMouseDown 55 8 V.BLeft [])
        expectAt "Nested popover" 10 15 fDown
        expectAbsent "Escape: close dialog" fDown

        fUp <- stepInput d (V.EvMouseUp 55 8 (Just V.BLeft))
        expectAbsent "Nested popover" fUp
        expectAt "Escape: close dialog" 11 15 fUp
        expectAt "Tier A dialog" 6 34 fUp
        expectAt "ab" 7 23 fUp
        expectAt "Second:" 8 15 fUp
        frameObservation fUp `shouldBe` 0
        fRetyped <- stepInput d (charKey 'Z')
        expectAt "abZ" 7 23 fRetyped
        take 3 (drop 23 (lineAt 8 fRetyped)) `shouldBe` "   "

        fClosed <- stepInput d escKey
        expectAt "count 0" 0 0 fClosed
        expectAbsent "Tier A dialog" fClosed
        stopApp d

  describe "resize" $ do
    it "preserves dialog contents and clips them to the viewport" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        _ <- stepInput d (charKey 'm')
        fTyped <- typeString d "hello"
        expectAt "hello" 7 23 fTyped
        _ <- stepInput d tabKey
        _ <- typeString d "world"

        fWide <- resizeApp d (100, 32)
        frameViewport fWide `shouldBe` (100, 32)
        expectAt "Tier A dialog" 10 44 fWide
        expectAt "First:" 11 25 fWide
        expectAt "hello" 11 33 fWide
        expectAt "Second:" 12 25 fWide
        expectAt "world" 12 33 fWide

        fNarrow <- resizeApp d (40, 12)
        frameViewport fNarrow `shouldBe` (40, 12)
        expectAt "Tier A dialog" 0 14 fNarrow
        expectAt "First:" 1 1 fNarrow
        expectAt "hello" 1 9 fNarrow
        expectAt "Second:" 2 1 fNarrow
        expectAt "p: open popover" 3 1 fNarrow
        expectAt "world" 2 9 fNarrow

        fBack <- resizeApp d (80, 24)
        frameViewport fBack `shouldBe` (80, 24)
        expectAt "Tier A dialog" 6 34 fBack
        expectAt "hello" 7 23 fBack
        expectAt "world" 8 23 fBack
        fFocused <- stepInput d (charKey 'Z')
        expectAt "worldZ" 8 23 fFocused

        fClosed <- stepInput d escKey
        expectAt "count 0" 0 0 fClosed
        expectAt "A" 9 1 fClosed
        stopApp d

  describe "shutdown routing" $ do
    it "background q emits exactly one shutdown" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        f <- stepInput d (charKey 'q')
        frameObservation f `shouldBe` 1
        stopApp d

    it "Ctrl+C exits from the background" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        f <- stepInput d ctrlC
        frameObservation f `shouldBe` 1
        stopApp d

    it "Ctrl+C exits from the dialog" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        fOpen <- stepInput d (charKey 'm')
        expectDialogAt80x24 fOpen
        f <- stepInput d ctrlC
        frameObservation f `shouldBe` 1
        stopApp d

    it "Ctrl+C exits from the nested popup" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        _ <- stepInput d (charKey 'm')
        _ <- stepInput d (charKey 'p')
        fOpen <- lastFrame d
        expectAt "Nested popover" 10 15 fOpen
        f <- stepInput d ctrlC
        frameObservation f `shouldBe` 1
        stopApp d
