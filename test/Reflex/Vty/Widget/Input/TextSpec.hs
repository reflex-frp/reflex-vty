{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description: Behavior-covering specs for the text input widgets,
-- driven through the production host.
module Reflex.Vty.Widget.Input.TextSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Data.Text.Zipper (fromText, left, textWidth, value)
import qualified Data.Text.Zipper as Z
import Reflex.Vty.Host
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..))
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Input.Text
import Reflex.Vty.Widget.Layout

spec :: Spec
spec = describe "Reflex.Vty.Widget.Input.Text" $ do
  describe "textInput" $ do
    it "types characters at the cursor" $
      withApp defaultVtyAppConfig (20, 3) (tiApp "" False) $ \driver -> do
        f1 <- stepInput driver (charKey 'a')
        let (v1, ls1, us1) = frameObservation f1
        v1 `shouldBe` "a"
        ls1 `shouldBe` 1
        us1 `shouldBe` ["a"]
        f2 <- stepInput driver (charKey 'b')
        frameObservation f2 `shouldBe` ("ab", 1, ["a", "ab"])
        gridRow (frameGrid f2) 0 `shouldBe` fullRow 20 "ab"
        stopApp driver

    it "backspace deletes left and delete deletes right" $
      withApp defaultVtyAppConfig (20, 3) (tiApp "" False) $ \driver -> do
        _ <- stepInput driver (charKey 'a')
        _ <- stepInput driver (charKey 'b')
        fBs <- stepInput driver (V.EvKey V.KBS [])
        let (v, _, _) = frameObservation fBs
        v `shouldBe` "a"
        _ <- stepInput driver (V.EvKey V.KHome [])
        fDel <- stepInput driver (V.EvKey V.KDel [])
        -- Home reports the (unchanged) value as a user-input
        -- occurrence because the zipper changed.
        frameObservation fDel `shouldBe` ("", 1, ["a", "ab", "a", "a", ""])
        gridRow (frameGrid fDel) 0 `shouldBe` fullRow 20 ""
        stopApp driver

    it "ctrl-u clears the input and ctrl-w deletes the previous word" $
      withApp defaultVtyAppConfig (20, 3) (tiApp "" False) $ \driver -> do
        typeStr driver "hello world"
        fW <- stepInput driver (ctrl 'w')
        let (vw, _, _) = frameObservation fW
        vw `shouldBe` "hello "
        fU <- stepInput driver (ctrl 'u')
        let (vu, lsu, usu) = frameObservation fU
        vu `shouldBe` ""
        lsu `shouldBe` 1
        usu `shouldSatisfy` ("" `elem`)
        gridRow (frameGrid fU) 0 `shouldBe` fullRow 20 ""
        stopApp driver

    it "left arrow moves the cursor within the line" $
      withApp defaultVtyAppConfig (20, 3) (tiApp "" False) $ \driver -> do
        typeStr driver "ab"
        _ <- stepInput driver (V.EvKey V.KLeft [])
        f <- stepInput driver (charKey 'X')
        let (v, _, _) = frameObservation f
        v `shouldBe` "aXb"
        gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "aXb"
        stopApp driver

    it "Enter does not insert a newline in a single-line input" $
      withApp defaultVtyAppConfig (20, 3) (tiApp "" False) $ \driver -> do
        typeStr driver "ab"
        _ <- stepInput driver (V.EvKey V.KEnter [])
        f <- stepInput driver (charKey 'c')
        let (v, ls, _) = frameObservation f
        v `shouldBe` "abc"
        ls `shouldBe` 1
        gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "abc"
        stopApp driver

    it "rejects Tab when acceptTab is False" $
      withApp defaultVtyAppConfig (20, 3) (tiApp "" False) $ \driver -> do
        _ <- stepInput driver (charKey 'a')
        _ <- stepInput driver (charKey 'b')
        fTab <- stepInput driver (V.EvKey (V.KChar '\t') [])
        let (v, _, us) = frameObservation fTab
        v `shouldBe` "ab"
        us `shouldBe` ["a", "ab"]
        f <- stepInput driver (charKey 'c')
        let (v', _, _) = frameObservation f
        v' `shouldBe` "abc"
        stopApp driver

    it "inserts a tab to the next tab stop when acceptTab is True" $
      withApp defaultVtyAppConfig (20, 3) (tiApp "" True) $ \driver -> do
        _ <- stepInput driver (charKey 'a')
        _ <- stepInput driver (charKey 'b')
        f <- stepInput driver (V.EvKey (V.KChar '\t') [])
        let (v, _, us) = frameObservation f
        v `shouldBe` "ab  "
        last us `shouldBe` "ab  "
        stopApp driver

    it "accepts UTF-8 paste including a wide character" $
      withApp defaultVtyAppConfig (20, 3) (tiApp "" False) $ \driver -> do
        f <- stepInput driver (V.EvPaste (TE.encodeUtf8 "hél世"))
        let (v, ls, us) = frameObservation f
        v `shouldBe` "hél世"
        ls `shouldBe` 1
        us `shouldBe` ["hél世"]
        gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "hél世"
        stopApp driver

    it "starts from the initial zipper with its cursor position" $
      withApp defaultVtyAppConfig (20, 3) (tiApp (left (fromText "abc")) False) $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f0 : _) -> do
            frameObservation f0 `shouldBe` ("abc", 1, [])
            gridRow (frameGrid f0) 0 `shouldBe` fullRow 20 "abc"
          [] -> expectationFailure "expected an initial frame"
        f <- stepInput driver (charKey 'X')
        let (v, _, _) = frameObservation f
        v `shouldBe` "abXc"
        gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "abXc"
        stopApp driver

    it "positions the cursor on click" $
      withApp defaultVtyAppConfig (20, 3) (tiApp (fromText "abcdef") False) $ \driver -> do
        fClick <- stepInput driver (V.EvMouseDown 3 0 V.BLeft [])
        let (vc, lsc, usc) = frameObservation fClick
        vc `shouldBe` "abcdef"
        lsc `shouldBe` 1
        usc `shouldBe` ["abcdef"]
        gridRow (frameGrid fClick) 0 `shouldBe` fullRow 20 "abcdef"
        f <- stepInput driver (charKey 'X')
        let (v, _, _) = frameObservation f
        v `shouldBe` "abcXdef"
        gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "abcXdef"
        stopApp driver

    it "masks the display while keeping the raw value" $
      withApp defaultVtyAppConfig (20, 3) maskApp $ \driver -> do
        typeStr driver "secret"
        fs <- driverFrames driver
        case reverse fs of
          (f : _) -> do
            let (v, _, us) = frameObservation f
            v `shouldBe` "secret"
            last us `shouldBe` "secret"
            gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "******"
          [] -> expectationFailure "expected frames"
        stopApp driver

    it "scrolls horizontally-truncated content to keep the cursor visible" $
      withApp defaultVtyAppConfig (20, 1) (tiApp "" False) $ \driver -> do
        typeStr driver "abcdefghijklmnopqrstuvwxyz0123"
        fs <- driverFrames driver
        case reverse fs of
          (f : _) -> do
            let (v, ls, _) = frameObservation f
            v `shouldBe` "abcdefghijklmnopqrstuvwxyz0123"
            ls `shouldBe` 2
            gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "uvwxyz0123"
          [] -> expectationFailure "expected frames"
        stopApp driver

    it "applies caller modification without emitting userInput" $
      withApp defaultVtyAppConfig (20, 3) (tiApp "" False) $ \driver -> do
        f <- stepCommand driver "SET"
        frameObservation f `shouldBe` ("SET", 1, [])
        gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "SET"
        stopApp driver

    it "applies modify after a simultaneous UI keypress, reporting only the UI result" $
      withApp defaultVtyAppConfig (20, 3) simApp $ \driver -> do
        f <- stepCommand driver ()
        frameObservation f `shouldBe` ("SET", 1, ["y"])
        gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "SET"
        stopApp driver

  describe "multilineTextInput" $ do
    it "Enter inserts a newline and counts display lines" $
      withApp defaultVtyAppConfig (20, 3) (mtiApp "" False) $ \driver -> do
        typeStr driver "ab"
        _ <- stepInput driver (V.EvKey V.KEnter [])
        f <- stepInput driver (charKey 'c')
        let (v, ls, _) = frameObservation f
        v `shouldBe` "ab\nc"
        ls `shouldBe` 2
        gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "ab"
        gridRow (frameGrid f) 1 `shouldBe` fullRow 20 "c"
        stopApp driver

    it "arrow keys move the cursor between lines" $
      withApp defaultVtyAppConfig (20, 3) (mtiApp (fromText "one\ntwo\nthree") False) $ \driver -> do
        _ <- stepInput driver (V.EvKey V.KUp [])
        fUp <- stepInput driver (charKey 'X')
        let (vu, _, _) = frameObservation fUp
        vu `shouldBe` "one\ntwoX\nthree"
        _ <- stepInput driver (V.EvKey V.KDown [])
        fDown <- stepInput driver (charKey 'Y')
        frameObservation fDown `shouldSatisfy` (\(v, ls, _) -> v == "one\ntwoX\nthreYe" && ls == 3)
        gridRow (frameGrid fDown) 1 `shouldBe` fullRow 20 "twoX"
        gridRow (frameGrid fDown) 2 `shouldBe` fullRow 20 "threYe"
        stopApp driver

    it "Home and End move to the start and end of the current line" $
      withApp defaultVtyAppConfig (20, 3) (mtiApp (fromText "one\ntwo\nthree") False) $ \driver -> do
        _ <- stepInput driver (V.EvKey V.KHome [])
        fHome <- stepInput driver (charKey 'X')
        let (vh, _, _) = frameObservation fHome
        vh `shouldBe` "one\ntwo\nXthree"
        gridRow (frameGrid fHome) 2 `shouldBe` fullRow 20 "Xthree"
        _ <- stepInput driver (V.EvKey V.KEnd [])
        fEnd <- stepInput driver (charKey 'Y')
        let (ve, _, _) = frameObservation fEnd
        ve `shouldBe` "one\ntwo\nXthreeY"
        stopApp driver

    it "PageUp and PageDown move by the viewport height" $
      withApp defaultVtyAppConfig (20, 3) (mtiApp (fromText "one\ntwo\nthree") False) $ \driver -> do
        _ <- stepInput driver (V.EvKey V.KPageUp [])
        fUp <- stepInput driver (charKey 'T')
        let (vt, _, _) = frameObservation fUp
        vt `shouldBe` "oneT\ntwo\nthree"
        gridRow (frameGrid fUp) 0 `shouldBe` fullRow 20 "oneT"
        _ <- stepInput driver (V.EvKey V.KPageDown [])
        fDown <- stepInput driver (charKey 'B')
        let (vb, ls, _) = frameObservation fDown
        vb `shouldBe` "oneT\ntwo\nthrBee"
        ls `shouldBe` 3
        stopApp driver

  describe "textInputTile" $ do
    it "sizes a column-oriented tile to its line count and adds lines with input" $
      withApp defaultVtyAppConfig (20, 5) tileColApp $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f0 : _) -> do
            frameObservation f0 `shouldBe` ("one\ntwo", 2)
            gridRow (frameGrid f0) 0 `shouldBe` fullRow 20 "one"
            gridRow (frameGrid f0) 1 `shouldBe` fullRow 20 "two"
            gridRow (frameGrid f0) 2 `shouldBe` fullRow 20 ""
          [] -> expectationFailure "expected an initial frame"
        -- Click the tile to focus it; keyboard input only reaches focused tiles.
        _ <- stepInput driver (V.EvMouseDown 0 0 V.BLeft [])
        fEnter <- stepInput driver (V.EvKey V.KEnter [])
        frameObservation fEnter `shouldBe` ("\none\ntwo", 3)
        f <- stepInput driver (charKey 'Z')
        -- The click at (0,0) left the cursor at the start of the
        -- document; Enter inserted a line there and 'Z' typed on the
        -- second logical line.
        frameObservation f `shouldBe` ("\nZone\ntwo", 3)
        gridRow (frameGrid f) 0 `shouldBe` fullRow 20 ""
        gridRow (frameGrid f) 1 `shouldBe` fullRow 20 "Zone"
        gridRow (frameGrid f) 2 `shouldBe` fullRow 20 "two"
        gridRow (frameGrid f) 3 `shouldBe` fullRow 20 ""
        stopApp driver

    it "sizes a row-oriented tile to the fallback width" $
      withApp defaultVtyAppConfig (20, 3) tileRowApp $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f : _) -> do
            frameObservation f `shouldBe` ("one two", 2)
            gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "one"
            gridRow (frameGrid f) 1 `shouldBe` fullRow 20 "two"
            gridRow (frameGrid f) 2 `shouldBe` fullRow 20 ""
          [] -> expectationFailure "expected an initial frame"
        stopApp driver

-- | Build a single- or multiline input app whose command event sets the
-- value through '_textInputConfig_modify'.
inputApp
  :: (forall t m. TestWidget t m => TextInputConfig t -> m (TextInput t))
  -> Z.TextZipper
  -> Bool
  -> TestApp Text (Text, Int, [Text])
inputApp mk initial acceptTab cmd = do
  ti <-
    mk
      def
        { _textInputConfig_initialValue = initial
        , _textInputConfig_acceptTab = acceptTab
        , _textInputConfig_modify = const . fromText <$> cmd
        }
  userInputVals <- foldDyn (\x acc -> value x : acc) [] (_textInput_userInput ti)
  pure
    ( (,,) <$> _textInput_value ti <*> _textInput_lines ti <*> (reverse <$> userInputVals)
    , never
    )

tiApp, mtiApp :: Z.TextZipper -> Bool -> TestApp Text (Text, Int, [Text])
tiApp = inputApp textInput
mtiApp = inputApp multilineTextInput

-- | A masked single-line input: the raw value is observable, but every
-- character renders as @*@.
maskApp :: TestApp Text (Text, Int, [Text])
maskApp = inputApp (\c -> textInput c {_textInputConfig_display = pure (const '*')}) "" False

-- | Sends a UI 'y' keypress (injected into the input stream) and a
-- caller modification to @"SET"@ simultaneously, from one command.
simApp :: TestApp () (Text, Int, [Text])
simApp cmd = do
  let yKey = V.EvKey (V.KChar 'y') [] <$ cmd
  ti <-
    localInput (\i -> leftmost [i, yKey]) $
      textInput def {_textInputConfig_modify = const (fromText "SET") <$ cmd}
  userInputVals <- foldDyn (\x acc -> value x : acc) [] (_textInput_userInput ti)
  pure
    ( (,,) <$> _textInput_value ti <*> _textInput_lines ti <*> (reverse <$> userInputVals)
    , never
    )

-- | A column-oriented 'textInputTile' around a multiline input with two
-- lines.
tileColApp :: TestApp () (Text, Int)
tileColApp _cmd = do
  ti <-
    initManager_ $
      col $
        textInputTile
          (multilineTextInput def {_textInputConfig_initialValue = fromText "one\ntwo"})
          (pure 10)
  pure ((,) <$> _textInput_value ti <*> _textInput_lines ti, never)

-- | A row-oriented 'textInputTile' with fallback width 6 around a
-- multiline input.
tileRowApp :: TestApp () (Text, Int)
tileRowApp _cmd = do
  ti <-
    initManager_ $
      row $
        textInputTile
          (multilineTextInput def {_textInputConfig_initialValue = fromText "one two"})
          (pure 6)
  pure ((,) <$> _textInput_value ti <*> _textInput_lines ti, never)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

charKey :: Char -> V.Event
charKey c = V.EvKey (V.KChar c) []

ctrl :: Char -> V.Event
ctrl c = V.EvKey (V.KChar c) [V.MCtrl]

-- | Type each character of the string into the app.
typeStr :: AppDriver c o -> String -> IO ()
typeStr driver = mapM_ (stepInput driver . charKey)

-- | The text of the @y@-th rendered row of a grid.
gridRow :: Grid -> Int -> String
gridRow g i = case drop i (rowStrings g) of
  (r : _) -> r
  [] -> ""

rowStrings :: Grid -> [String]
rowStrings = map (concatMap (T.unpack . renderSpanText)) . gridRows

-- | Pad a row string with trailing blanks to a full terminal row of the
-- given display width (wide characters count for their column width).
fullRow :: Int -> String -> String
fullRow w s = s ++ replicate (w - textWidth (T.pack s)) ' '
