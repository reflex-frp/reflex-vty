{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Acceptance coverage for the Articles demo.
--
-- The list contract is the heading, three ordered rows with the selection
-- drawn reverse-video, and the hint. Enter opens the selected article as a
-- full-screen reading view; Escape returns with the selection intact and
-- the article marked read. Read state is observed through the bullet the
-- renderer draws, so these tests exercise the production widget stack.
module Example.ArticlesSpec (spec) where

import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty (defaultVtyAppConfig)
import Test.Hspec

import Example.Articles (articlesWidget)
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (attrAt, gridRows, renderSpanText)

-- | Runs the production demo in the recording host while counting its
-- shutdown requests.
wrapDemo :: TestApp () Int
wrapDemo _ = do
  shutdown <- articlesWidget
  shutdownCount <- foldDyn (\() n -> n + 1) (0 :: Int) shutdown
  pure (shutdownCount, never)

type Demo = AppDriver () Int

lastFrame :: Demo -> IO (Frame Int)
lastFrame d = last <$> driverFrames d

-- * Screen helpers

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
  case [ (y, c)
       | (y, line) <- zip [0 ..] (linesOf f)
       , Just c <- [findColumn needle line]
       ] of
    (p : _) -> Just p
    [] -> Nothing

expectText :: String -> Frame Int -> Expectation
expectText needle f =
  case findText needle f of
    Just _ -> pure ()
    Nothing ->
      expectationFailure $
        "missing " ++ show needle ++ "\n" ++ numbered f

expectAbsent :: String -> Frame Int -> Expectation
expectAbsent needle f =
  case findText needle f of
    Nothing -> pure ()
    Just (y, x) ->
      expectationFailure $
        "unexpected " ++ show needle ++ " at " ++ show (y, x) ++ "\n" ++ numbered f

-- | Whether the cell where @row@ starts is drawn reverse-video.
reversedAt :: Int -> Int -> Frame Int -> Bool
reversedAt x y f =
  maybe
    False
    (\a -> V.hasStyle (V.fixedStyle a) V.reverseVideo)
    (attrAt x y (frameGrid f))

expectSelected :: String -> Frame Int -> Expectation
expectSelected row f =
  case findText row f of
    Just (y, x) -> reversedAt x y f `shouldBe` True
    Nothing ->
      expectationFailure $
        "missing selected row " ++ show row ++ "\n" ++ numbered f

expectNotSelected :: String -> Frame Int -> Expectation
expectNotSelected row f =
  case findText row f of
    Just (y, x) -> reversedAt x y f `shouldBe` False
    Nothing ->
      expectationFailure $
        "missing unselected row " ++ show row ++ "\n" ++ numbered f

-- * Input helpers

charKey :: Char -> V.Event
charKey c = V.EvKey (V.KChar c) []

escKey, enterKey, upKey, downKey :: V.Event
escKey = V.EvKey V.KEsc []
enterKey = V.EvKey V.KEnter []
upKey = V.EvKey V.KUp []
downKey = V.EvKey V.KDown []

ctrlC :: V.Event
ctrlC = V.EvKey (V.KChar 'c') [V.MCtrl]

-- * Expectations

expectList :: Frame Int -> Expectation
expectList f = do
  expectText "Articles" f
  expectText "Up/Down: Select | Enter: Read | q: Quit" f
  frameObservation f `shouldBe` 0

expectReading :: String -> String -> Frame Int -> Expectation
expectReading title bodyLine f = do
  expectText title f
  expectText bodyLine f
  expectText "Escape: Back" f
  expectAbsent "Articles" f
  expectAbsent "Up/Down: Select | Enter: Read | q: Quit" f
  expectAbsent "Grid layout tips" f
  frameObservation f `shouldBe` 0

-- * Tests

spec :: Spec
spec = describe "Example.Articles" $ do
  describe "list" $ do
    it "shows three unread rows and opens the initially selected first article" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        f0 <- lastFrame d
        expectList f0
        expectText "● Reflex 1.3 released" f0
        expectText "● Terminal scrolling deep dive" f0
        expectText "● Grid layout tips" f0
        expectAbsent "○" f0
        expectSelected "● Reflex 1.3 released" f0
        expectNotSelected "● Terminal scrolling deep dive" f0

        fRead <- stepInput d enterKey
        expectReading "Reflex 1.3 released" "Reflex 1.3 is out." fRead
        stopApp d

    it "opens the second article after Down and marks it read on return" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        fDown <- stepInput d downKey
        expectSelected "● Terminal scrolling deep dive" fDown
        expectNotSelected "● Reflex 1.3 released" fDown

        fRead <- stepInput d enterKey
        expectReading
          "Terminal scrolling deep dive"
          "Scrolling is more than moving text."
          fRead

        fBack <- stepInput d escKey
        expectList fBack
        expectText "○ Terminal scrolling deep dive" fBack
        expectText "● Reflex 1.3 released" fBack
        expectText "● Grid layout tips" fBack
        expectAbsent "● Terminal scrolling deep dive" fBack
        expectSelected "○ Terminal scrolling deep dive" fBack
        expectNotSelected "● Reflex 1.3 released" fBack
        stopApp d

  describe "input routing" $ do
    it "ignores q in the reading view and quits once from the list" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        _ <- stepInput d downKey
        _ <- stepInput d enterKey

        fQ <- stepInput d (charKey 'q')
        expectText "Terminal scrolling deep dive" fQ
        expectText "Escape: Back" fQ
        expectAbsent "Up/Down: Select | Enter: Read | q: Quit" fQ
        frameObservation fQ `shouldBe` 0

        _ <- stepInput d escKey
        fQuit <- stepInput d (charKey 'q')
        frameObservation fQuit `shouldBe` 1
        stopApp d

  describe "shutdown routing" $ do
    it "Ctrl+C exits from the list view" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        f <- stepInput d ctrlC
        frameObservation f `shouldBe` 1
        stopApp d

    it "Ctrl+C exits from the reading view" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        _ <- stepInput d downKey
        _ <- stepInput d enterKey
        f <- stepInput d ctrlC
        frameObservation f `shouldBe` 1
        stopApp d

  describe "state retention" $ do
    it "keeps selection and read state across repeated open and return" $
      withApp defaultVtyAppConfig (80, 24) wrapDemo $ \d -> do
        _ <- stepInput d downKey
        fOpen <- stepInput d enterKey
        expectText "Escape: Back" fOpen
        expectText "Terminal scrolling deep dive" fOpen

        fBack <- stepInput d escKey
        expectText "○ Terminal scrolling deep dive" fBack
        expectText "● Reflex 1.3 released" fBack
        expectSelected "○ Terminal scrolling deep dive" fBack

        _ <- stepInput d enterKey
        fBackAgain <- stepInput d escKey
        expectText "○ Terminal scrolling deep dive" fBackAgain
        expectText "● Reflex 1.3 released" fBackAgain
        expectSelected "○ Terminal scrolling deep dive" fBackAgain
        frameObservation fBackAgain `shouldBe` 0

        _ <- stepInput d upKey
        _ <- stepInput d enterKey
        fFirstBack <- stepInput d escKey
        expectText "○ Reflex 1.3 released" fFirstBack
        expectText "○ Terminal scrolling deep dive" fFirstBack
        expectText "● Grid layout tips" fFirstBack
        expectSelected "○ Reflex 1.3 released" fFirstBack
        expectNotSelected "○ Terminal scrolling deep dive" fFirstBack
        frameObservation fFirstBack `shouldBe` 0
        stopApp d
