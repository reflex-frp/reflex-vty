{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Acceptance coverage for the production note-editor demo.
--
-- Every scenario runs 'Example.TierA.demoWidget' through the recording
-- production-stack harness. The demo's returned shutdown event feeds a
-- counter that is exposed as the observation, so quit routing is asserted
-- without stopping the harness host (whose own shutdown stays 'never').
--
-- The background contract is the committed note on one row and the
-- background-only controls below it. The modal contract is the title, the
-- draft field, and the save\/cancel hint. Drafts are checked through the
-- actual text-input rendering, including the cancel-and-reopen reset.
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

expectText :: String -> Frame Int -> Expectation
expectText needle f =
  case findText needle f of
    Just _ -> pure ()
    Nothing ->
      expectationFailure $
        "missing " ++ show needle ++ "\n" ++ numbered f

-- | Assert a single-line widget is horizontally centered with the floor of
-- the slack on the left. This is the demo's explicit centering contract; it
-- does not rely on the coincidental column of a previous rendering.
expectCentered :: String -> Int -> Frame Int -> Expectation
expectCentered needle y f =
  expectAt needle y ((fst (frameViewport f) - length needle) `div` 2) f

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

bsKey, escKey, enterKey :: V.Event
bsKey = V.EvKey V.KBS []
escKey = V.EvKey V.KEsc []
enterKey = V.EvKey V.KEnter []

ctrlC :: V.Event
ctrlC = V.EvKey (V.KChar 'c') [V.MCtrl]

-- | Send a batch of keys, returning the final fenced frame.
send :: Demo -> [V.Event] -> IO (Frame Int)
send d es = do
  f0 <- lastFrame d
  foldM (\_ e -> stepInput d e) f0 es

typeString :: Demo -> String -> IO (Frame Int)
typeString d = send d . map charKey

-- | Every draft opens with the cursor at the end, so six backspaces alone
-- erase @Friday@ (the recording's End key is harmless on top of this).
replaceFridayWithMonday :: Demo -> IO (Frame Int)
replaceFridayWithMonday d = do
  f0 <- send d (replicate 6 bsKey)
  expectAbsent "Friday" f0
  typeString d "Monday"

-- | The background note and controls at the baseline 80x24 viewport.
expectBackgroundFriday :: Frame Int -> Expectation
expectBackgroundFriday f = do
  expectCentered "Note: Ship on Friday" 11 f
  expectCentered "e: Edit | q: Quit" 12 f
  expectAbsent "Edit note" f
  expectAbsent "Enter: Save | Escape: Cancel" f
  frameObservation f `shouldBe` 0

-- | A 40x5 panel centred in 80x24: top-left (20,9), title at (36,9),
-- body at (21,10), and its hint on the next body row.
expectDialogFriday :: Frame Int -> Expectation
expectDialogFriday f = do
  expectAt "Edit note" 9 36 f
  expectAt "Ship on Friday" 10 21 f
  expectAt "Enter: Save | Escape: Cancel" 11 21 f
  expectAbsent "e: Edit | q: Quit" f
  frameObservation f `shouldBe` 0

spec :: Spec
spec = describe "Example.TierA" $ do
  describe "background" $ do
    it "shows the committed note and only the background controls" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        f0 <- lastFrame d
        expectBackgroundFriday f0
        stopApp d

  describe "draft lifecycle" $ do
    it "discards a canceled draft and resets from the committed note on reopen" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        fOpen <- stepInput d (charKey 'e')
        expectDialogFriday fOpen

        fDraft <- replaceFridayWithMonday d
        expectAt "Ship on Monday" 10 21 fDraft
        expectAbsent "Ship on Friday" fDraft
        frameObservation fDraft `shouldBe` 0

        fCancel <- stepInput d escKey
        expectBackgroundFriday fCancel
        expectAbsent "Monday" fCancel

        fReopen <- stepInput d (charKey 'e')
        expectAt "Ship on Friday" 10 21 fReopen
        expectAbsent "Monday" fReopen
        -- The reset draft also has its cursor at the end.
        fEraseAgain <- send d (replicate 6 bsKey)
        expectAbsent "Friday" fEraseAgain
        fCancelAgain <- stepInput d escKey
        expectBackgroundFriday fCancelAgain
        frameObservation fReopen `shouldBe` 0
        stopApp d

    it "commits an edited note on Enter and offers it as the next draft" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        _ <- stepInput d (charKey 'e')
        fDraft <- replaceFridayWithMonday d
        expectAt "Ship on Monday" 10 21 fDraft

        fSaved <- stepInput d enterKey
        expectCentered "Note: Ship on Monday" 11 fSaved
        expectAt "e: Edit | q: Quit" 12 31 fSaved
        expectAbsent "Edit note" fSaved
        expectAbsent "Friday" fSaved
        frameObservation fSaved `shouldBe` 0

        fReopen <- stepInput d (charKey 'e')
        expectAt "Ship on Monday" 10 21 fReopen
        expectAbsent "Friday" fReopen
        -- Cancel after reopening leaves the committed note intact.
        fCancel <- stepInput d escKey
        expectCentered "Note: Ship on Monday" 11 fCancel
        frameObservation fCancel `shouldBe` 0
        stopApp d

  describe "input isolation" $ do
    it "types e and q into the dialog while q still quits only from the background" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        _ <- stepInput d (charKey 'e')
        fQ <- stepInput d (charKey 'q')
        expectAt "Ship on Fridayq" 10 21 fQ
        expectAt "Edit note" 9 36 fQ
        frameObservation fQ `shouldBe` 0

        fE <- stepInput d (charKey 'e')
        expectAt "Ship on Fridayqe" 10 21 fE
        expectAt "Edit note" 9 36 fE
        frameObservation fE `shouldBe` 0

        fCancel <- stepInput d escKey
        expectCentered "Note: Ship on Friday" 11 fCancel
        expectAbsent "Fridayq" fCancel
        frameObservation fCancel `shouldBe` 0

        fQuit <- stepInput d (charKey 'q')
        frameObservation fQuit `shouldBe` 1
        stopApp d

  describe "resize" $ do
    it "preserves draft text and focus across viewport changes and saves after" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        _ <- stepInput d (charKey 'e')
        fDraft <- replaceFridayWithMonday d
        expectAt "Ship on Monday" 10 21 fDraft

        fWide <- resizeApp d (100, 32)
        frameViewport fWide `shouldBe` (100, 32)
        expectText "Ship on Monday" fWide
        expectText "Edit note" fWide
        expectText "Enter: Save | Escape: Cancel" fWide

        fSmall <- resizeApp d (40, 12)
        frameViewport fSmall `shouldBe` (40, 12)
        expectText "Ship on Monday" fSmall
        expectText "Enter: Save | Escape: Cancel" fSmall

        fBack <- resizeApp d (80, 24)
        frameViewport fBack `shouldBe` (80, 24)
        expectAt "Ship on Monday" 10 21 fBack

        fBang <- stepInput d (charKey '!')
        expectAt "Ship on Monday!" 10 21 fBang
        fSaved <- stepInput d enterKey
        expectCentered "Note: Ship on Monday!" 11 fSaved
        expectAbsent "Edit note" fSaved
        frameObservation fSaved `shouldBe` 0
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

    it "Ctrl+C exits from the dialog without committing" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        _ <- stepInput d (charKey 'e')
        fDraft <- replaceFridayWithMonday d
        expectAt "Ship on Monday" 10 21 fDraft
        f <- stepInput d ctrlC
        frameObservation f `shouldBe` 1
        stopApp d
