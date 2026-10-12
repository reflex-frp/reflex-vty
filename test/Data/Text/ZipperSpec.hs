{-# LANGUAGE OverloadedStrings #-}

module Data.Text.ZipperSpec
  ( spec
  ) where

import Control.Monad
import Data.Char (isSpace)
import Data.List (mapAccumL)
import qualified Data.Map as Map
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck (conjoin, counterexample, forAll)
import Prelude

import Data.Text.Zipper
import Reflex.Vty.Test.Generators
  ( EditOp (..)
  , genEditScript
  , genMixedWidthText
  , runPlanProperty
  )

someSentence :: T.Text
someSentence = "12345 1234 12"

newlineSentence :: T.Text
newlineSentence = "\n\n\n\n\n"

splitSentenceAtDisplayWidth :: Int -> T.Text -> [(T.Text, Bool)]
splitSentenceAtDisplayWidth w t = splitWordsAtDisplayWidth w (wordsWithWhitespace t)

spec :: Spec
spec =
  describe "Zipper" $ do
    it "wordsWithWhitespace" $ do
      wordsWithWhitespace "" `shouldBe` []
      wordsWithWhitespace "😱😱😱" `shouldBe` ["😱😱😱"]
      wordsWithWhitespace "abcd efgf f" `shouldBe` ["abcd ", "efgf ", "f"]
      wordsWithWhitespace "aoeu    " `shouldBe` ["aoeu    "]
    it "splitWordsAtDisplayWidth" $ do
      fmap fst (splitSentenceAtDisplayWidth 5 "123456") `shouldBe` ["12345", "6"]
      fmap fst (splitSentenceAtDisplayWidth 5 "12345 6") `shouldBe` ["12345", "6"]
      fmap fst (splitSentenceAtDisplayWidth 5 "1234 56") `shouldBe` ["1234", "56"]
      fmap fst (splitSentenceAtDisplayWidth 5 "12345678912345") `shouldBe` ["12345", "67891", "2345"]
      fmap fst (splitSentenceAtDisplayWidth 5 "1234   56") `shouldBe` ["1234 ", " 56"]
      fmap fst (splitSentenceAtDisplayWidth 8 "1 2 3 4 5 6 7 8 9 1") `shouldBe` ["1 2 3 4", "5 6 7 8", "9 1"]
    it "wrapWithOffsetAndAlignment" $ do
      wrapWithOffsetAndAlignment TextAlignment_Left 5 0 someSentence `shouldBe` [(WrappedLine "12345" True 0), (WrappedLine "1234" True 0), (WrappedLine "12" False 0)]
      wrapWithOffsetAndAlignment TextAlignment_Right 5 0 someSentence `shouldBe` [(WrappedLine "12345" True 0), (WrappedLine "1234" True 1), (WrappedLine "12" False 3)]
      wrapWithOffsetAndAlignment TextAlignment_Center 5 0 someSentence `shouldBe` [(WrappedLine "12345" True 0), (WrappedLine "1234" True 0), (WrappedLine "12" False 1)]
      wrapWithOffsetAndAlignment TextAlignment_Left 5 1 someSentence `shouldBe` [(WrappedLine "1234" False 0), (WrappedLine "5" True 0), (WrappedLine "1234" True 0), (WrappedLine "12" False 0)]

    it "eolSpacesToLogicalLines" $ do
      eolSpacesToLogicalLines
        [ [(WrappedLine "😱" True 1), (WrappedLine "😱" False 2), (WrappedLine "😱" False 3)]
        , [(WrappedLine "aa" True 1), (WrappedLine "aa" True 2), (WrappedLine "aa" False 3)]
        ]
        `shouldBe` [ [("😱", 1)]
                   , [("😱", 2), ("😱", 3)]
                   , [("aa", 1)]
                   , [("aa", 2)]
                   , [("aa", 3)]
                   ]
    it "offsetMapWithAlignmentInternal" $ do
      offsetMapWithAlignmentInternal
        [ [(WrappedLine "😱" True 1), (WrappedLine "😱" False 2), (WrappedLine "😱" False 3)]
        , [(WrappedLine "aa" True 1), (WrappedLine "aa" True 2), (WrappedLine "aa" False 3)]
        ]
        `shouldBe` Map.fromList
          [ (0, (1, 0))
          , (1, (2, 2)) -- jump by 1 for char and 1 for space
          , (2, (3, 3)) -- jump by 1 for char
          , (3, (1, 5)) -- jump by 1 for char and 1 for newline
          , (4, (2, 8)) -- jump by 2 for char and 1 for space
          , (5, (3, 11)) -- jump by 2 for char and 1 for space
          ]
    it "displayLinesWithAlignment - spans" $ do
      let makespans = fmap (fmap (Span ()))
          insertcharnewlinesentence = insertChar '\n' $ insertChar '\n' $ insertChar '\n' $ insertChar '\n' $ insertChar '\n' $ fromText ""
          cursorspan = [[Span () " "]]
          -- newline cases
          dl0 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText newlineSentence)
          dl1 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "aoeu\n\n\naoeu")
          dl2 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "\n\n\naoeu")
          dl3 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "aoeu\n\n\n")
          dl4 = displayLinesWithAlignment TextAlignment_Right 10 () () (empty)

      insertcharnewlinesentence `shouldBe` fromText newlineSentence

      -- NOTE the trailing " " is the generated cursor cell: when the cursor
      -- sits past the last character of its line, it is rendered as a blank
      -- cell carrying the cursor tag so it stays visible.
      _displayLines_spans dl0 `shouldBe` makespans [[""], [""], [""], [""], [""], [" "]]
      _displayLines_spans dl1 `shouldBe` makespans [["aoeu"], [""], [""], ["aoeu", " "]]
      _displayLines_spans dl2 `shouldBe` makespans [[""], [""], [""], ["aoeu", " "]]
      _displayLines_spans dl3 `shouldBe` makespans [["aoeu"], [""], [""], [" "]]
      _displayLines_spans dl4 `shouldBe` makespans [[" "]]

    it "displayLinesWithAlignment - cursor tag" $ do
      let dl0 = displayLinesWithAlignment TextAlignment_Right 10 0 1 (fromText "abc")
          dl1 = displayLinesWithAlignment TextAlignment_Right 10 0 1 empty
          -- cursor moved left into the text: it lands on a real character
          dl2 = displayLinesWithAlignment TextAlignment_Right 10 0 1 (left (fromText "abc"))
      -- cursor at end of text and on an empty line render a blank cursor cell
      -- so the cursor remains visible (see Data.Text.Zipper cursorcell)
      _displayLines_spans dl0 `shouldBe` [[Span 0 "abc", Span 1 " "]]
      _displayLines_spans dl1 `shouldBe` [[Span 1 " "]]
      -- when the cursor is over a character, that character carries the tag
      _displayLines_spans dl2 `shouldBe` [[Span 0 "ab", Span 1 "c"]]

    it "displayLines - cursorPos" $ do
      let dl0 = displayLinesWithAlignment TextAlignment_Left 10 () () (fromText "")
          dl1 = displayLinesWithAlignment TextAlignment_Left 10 () () (fromText "aoeu")
          dl2 = displayLinesWithAlignment TextAlignment_Left 10 () () (fromText "aoeu\n")
          dl3 = displayLinesWithAlignment TextAlignment_Left 10 () () (fromText "0123456789")
          dl4 = displayLinesWithAlignment TextAlignment_Left 10 () () (insertChar 'a' $ fromText "aoeu")
          dl5 = displayLinesWithAlignment TextAlignment_Left 10 () () (left $ insertChar 'a' $ fromText "aoeu")
          dl6 = displayLinesWithAlignment TextAlignment_Left 10 () () (deleteLeft $ insertChar 'a' $ fromText "aoeu")
          dl7 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "")

      _displayLines_cursorPos dl0 `shouldBe` (0, 0)
      _displayLines_cursorPos dl1 `shouldBe` (4, 0)
      _displayLines_cursorPos dl2 `shouldBe` (0, 1)
      _displayLines_cursorPos dl3 `shouldBe` (0, 1)
      _displayLines_cursorPos dl4 `shouldBe` (5, 0)
      _displayLines_cursorPos dl5 `shouldBe` (4, 0)
      _displayLines_cursorPos dl6 `shouldBe` (4, 0)
      _displayLines_cursorPos dl7 `shouldBe` (10, 0)
    it "displayLinesWithAlignment - spans" $ do
      let someText = top $ fromText "0123456789abcdefgh"
      -- outer span length should be invariant when changing TextAlignment and CursorPosition
      -- print $ displayLinesWithAlignment TextAlignment_Left 5 () () someText
      forM_ [0 .. 4] $ \x -> do
        forM_ [TextAlignment_Left, TextAlignment_Center, TextAlignment_Right] $ \ta -> do
          let t = rightN x $ someText
          length (_displayLines_spans $ (displayLinesWithAlignment ta 5 () () t)) `shouldBe` 4

    describe "editing against a flat-text model" $ do
      it "contents and cursor index agree after every operation of a random script" $
        runPlanProperty $
          forAll ((,) <$> genMixedWidthText <*> genEditScript) $ \(t, script) ->
            let walk (z, m) op =
                  ( (applyZipper op z, applyModel op m)
                  , let z' = applyZipper op z
                        (flat', idx') = applyModel op m
                    in counterexample
                         ( "after op "
                             ++ show op
                             ++ ": zipper value "
                             ++ show (value z')
                             ++ " vs model "
                             ++ show flat'
                             ++ "; marked zipper "
                             ++ show (value (insert "^" z'))
                             ++ " vs marked model "
                             ++ show (T.take idx' flat' <> "^" <> T.drop idx' flat')
                         )
                         ( value z' == flat'
                             && value (insert "^" z')
                               == T.take idx' flat' <> "^" <> T.drop idx' flat'
                         )
                  )
                (_, checks) =
                  mapAccumL
                    walk
                    -- 'fromText' places the cursor at the end of the
                    -- document.
                    (fromText t, (t, T.length t))
                    script
            in conjoin checks

      it "line joining, splitting, empty documents, and boundaries" $ do
        -- Directed boundary cases for the same model.
        let model z m = value z == fst m && value (insert "^" z) == markerOf m
            markerOf (flat, idx) = T.take idx flat <> "^" <> T.drop idx flat
        model (deleteLeft (insertChar 'a' (fromText ""))) (applyModel OpDeleteLeft (applyModel (OpInsertChar 'a') ("", 0)))
          `shouldBe` True
        model (up (fromText "ab\ncd")) (applyModel OpUp ("ab\ncd", 5)) `shouldBe` True
        model (down (up (fromText "ab\ncd"))) (applyModel OpDown (applyModel OpUp ("ab\ncd", 5))) `shouldBe` True
        model (deleteLeftWord (fromText "ab cd  ")) (applyModel OpDeleteLeftWord ("ab cd  ", 7)) `shouldBe` True
        model (tab 4 (fromText "ab")) (applyModel (OpTab 4) ("ab", 2)) `shouldBe` True
        model (pageUp 0 (fromText "a\nb")) (applyModel (OpPageUp 0) ("a\nb", 3)) `shouldBe` True
        model (pageDown (-2) (fromText "a\nb")) (applyModel (OpPageDown (-2)) ("a\nb", 3)) `shouldBe` True

applyZipper :: EditOp -> TextZipper -> TextZipper
applyZipper op z = case op of
  OpLeft -> left z
  OpRight -> right z
  OpUp -> up z
  OpDown -> down z
  OpHome -> home z
  OpEnd -> end z
  OpPageUp n -> pageUp n z
  OpPageDown n -> pageDown n z
  OpInsertChar c -> insertChar c z
  OpInsertNewline -> insertChar '\n' z
  OpInsertText s -> insert s z
  OpDeleteLeft -> deleteLeft z
  OpDeleteRight -> deleteRight z
  OpDeleteLeftWord -> deleteLeftWord z
  OpTab n -> tab n z
  OpTop -> top z

-- | The independent model: flat text plus a logical character index.
type FlatModel = (T.Text, Int)

dropTrailing :: (Char -> Bool) -> T.Text -> T.Text
dropTrailing p = T.dropWhileEnd p

applyModel :: EditOp -> FlatModel -> FlatModel
applyModel op (flat, idx) = case op of
  OpLeft -> (flat, max 0 (idx - 1))
  OpRight -> (flat, min (T.length flat) (idx + 1))
  OpUp -> (flat, moveLine flat idx (-1))
  OpDown -> (flat, moveLine flat idx 1)
  OpHome ->
    let start = lineStart flat idx
    in (flat, start)
  OpEnd ->
    let ls = T.splitOn "\n" flat
        (ln, _) = lineCol flat idx
        endIdx = lineStart flat idx + T.length (ls !! ln)
    in (flat, endIdx)
  OpPageUp 0 -> (flat, idx)
  OpPageUp n -> (flat, iterate moveUp idx !! max 0 n)
    where
      moveUp i = moveLine flat i (-1)
  OpPageDown 0 -> (flat, idx)
  OpPageDown n -> (flat, iterate moveDown idx !! max 0 n)
    where
      moveDown i = moveLine flat i 1
  OpInsertChar c -> (T.take idx flat <> T.singleton c <> T.drop idx flat, idx + 1)
  OpInsertNewline -> (T.take idx flat <> "\n" <> T.drop idx flat, idx + 1)
  OpInsertText s ->
    ( T.take idx flat <> s <> T.drop idx flat
    , idx + T.length s
    )
  OpDeleteLeft
    | idx > 0 -> (T.take (idx - 1) flat <> T.drop idx flat, idx - 1)
    | otherwise -> (flat, idx)
  OpDeleteRight
    | idx < T.length flat -> (T.take idx flat <> T.drop (idx + 1) flat, idx)
    | otherwise -> (flat, idx)
  OpDeleteLeftWord ->
    let prefix = T.take idx flat
        suffix = T.drop idx flat
        p1 = dropTrailing isSpace prefix
        p2 = dropTrailing (not . isSpace) p1
    in (p2 <> suffix, T.length p2)
  OpTab n ->
    let n' = max 1 n
        lb = idx - lineStart flat idx
        k = n - lb `mod` n'
        k' = max 0 k
    in ( T.take idx flat <> T.replicate k' " " <> T.drop idx flat
       , idx + k'
       )
  OpTop -> (flat, 0)

lineStart :: T.Text -> Int -> Int
lineStart flat idx =
  let prefix = T.take idx flat
      breaks = T.findIndex (== '\n') (T.reverse prefix)
  in case breaks of
       Nothing -> 0
       Just k -> idx - k

lineCol :: T.Text -> Int -> (Int, Int)
lineCol flat idx = (ln, col)
  where
    start = lineStart flat idx
    ln = T.count "\n" (T.take start flat)
    col = idx - start

moveLine :: T.Text -> Int -> Int -> Int
moveLine flat idx delta =
  let ls = T.splitOn "\n" flat
      (ln, col) = lineCol flat idx
      ln' = ln + delta
  in if ln' < 0 || ln' >= length ls
       then idx
       else lineStartOf ln' + min col (T.length (ls !! ln'))
  where
    lineStartOf l = sum (map (\s -> T.length s + 1) (take l (T.splitOn "\n" flat)))
