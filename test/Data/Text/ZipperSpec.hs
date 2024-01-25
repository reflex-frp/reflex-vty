{-# LANGUAGE OverloadedStrings #-}

module Data.Text.ZipperSpec (
  spec
) where

import           Prelude

import           Test.Hspec

import qualified Data.Map         as Map
import qualified          Data.Text as T
import Control.Monad

import           Data.Text.Zipper

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
    wordsWithWhitespace "abcd efgf f" `shouldBe` ["abcd ","efgf ", "f"]
    wordsWithWhitespace "aoeu    " `shouldBe` ["aoeu    "]
  it "splitWordsAtDisplayWidth" $ do
    fmap fst (splitSentenceAtDisplayWidth 5 "123456") `shouldBe` ["12345","6"]
    fmap fst (splitSentenceAtDisplayWidth 5 "12345 6") `shouldBe` ["12345","6"]
    fmap fst (splitSentenceAtDisplayWidth 5 "1234 56") `shouldBe` ["1234","56"]
    fmap fst (splitSentenceAtDisplayWidth 5 "12345678912345") `shouldBe` ["12345","67891","2345"]
    fmap fst (splitSentenceAtDisplayWidth 5 "1234   56") `shouldBe` ["1234 "," 56"]
    fmap fst (splitSentenceAtDisplayWidth 8 "1 2 3 4 5 6 7 8 9 1") `shouldBe` ["1 2 3 4","5 6 7 8", "9 1"]
  it "wrapWithOffsetAndAlignment" $ do
    wrapWithOffsetAndAlignment TextAlignment_Left 5 0 someSentence `shouldBe` [(WrappedLine "12345" True 0), (WrappedLine "1234" True 0), (WrappedLine "12" False 0)]
    wrapWithOffsetAndAlignment TextAlignment_Right 5 0 someSentence `shouldBe` [(WrappedLine "12345" True 0), (WrappedLine "1234" True 1), (WrappedLine "12" False 3)]
    wrapWithOffsetAndAlignment TextAlignment_Center 5 0 someSentence `shouldBe` [(WrappedLine "12345" True 0), (WrappedLine "1234" True 0), (WrappedLine "12" False 1)]
    wrapWithOffsetAndAlignment TextAlignment_Left 5 1 someSentence `shouldBe` [(WrappedLine "1234" False 0), (WrappedLine "5" True 0), (WrappedLine "1234" True 0), (WrappedLine "12" False 0)]

  it "eolSpacesToLogicalLines" $ do
    eolSpacesToLogicalLines
      [
        [ (WrappedLine "😱" True 1), (WrappedLine "😱" False 2), (WrappedLine "😱" False 3) ]
        , [ (WrappedLine "aa" True 1), (WrappedLine "aa" True 2), (WrappedLine "aa" False 3) ]
      ]
      `shouldBe`
      [
        [ ("😱",1) ]
        , [ ("😱",2), ("😱",3) ]
        , [ ("aa",1) ]
        , [ ("aa",2) ]
        , [ ("aa",3) ]
      ]
  it "offsetMapWithAlignmentInternal" $ do
    offsetMapWithAlignmentInternal
      [
        [ (WrappedLine "😱" True 1), (WrappedLine "😱" False 2), (WrappedLine "😱" False 3) ]
        , [ (WrappedLine "aa" True 1), (WrappedLine "aa" True 2), (WrappedLine "aa" False 3) ]
      ]
      `shouldBe`
      Map.fromList
      [ (0, (1,0))
      , (1, (2,2)) -- jump by 1 for char and 1 for space
      , (2, (3,3)) -- jump by 1 for char
      , (3, (1,5)) -- jump by 1 for char and 1 for newline
      , (4, (2,8)) -- jump by 2 for char and 1 for space
      , (5, (3,11)) -- jump by 2 for char and 1 for space
      ]
  it "displayLinesWithAlignment - spans" $ do
    let
      makespans = fmap (fmap (Span ()))
      insertcharnewlinesentence = insertChar '\n' $ insertChar '\n' $ insertChar '\n' $ insertChar '\n' $ insertChar '\n' $ fromText ""
      cursorspan = [[Span () " "]]
      -- newline cases
      dl0 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText newlineSentence)
      dl1 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "aoeu\n\n\naoeu")
      dl2 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "\n\n\naoeu")
      dl3 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "aoeu\n\n\n")
      dl4 = displayLinesWithAlignment TextAlignment_Right 10 () () (empty)


    insertcharnewlinesentence `shouldBe` fromText newlineSentence

    -- NOTE last " " is the generated cursor span char
    _displayLines_spans dl0 `shouldBe` makespans [[""],[""],[""],[""],[""],[""]]
    _displayLines_spans dl1 `shouldBe` makespans [["aoeu"],[""],[""],["aoeu", ""]]
    _displayLines_spans dl2 `shouldBe` makespans [[""],[""],[""],["aoeu", ""]]
    _displayLines_spans dl3 `shouldBe` makespans [["aoeu"],[""],[""],[""]]
    _displayLines_spans dl4 `shouldBe` makespans [[""]]

  
  it "displayLinesWithAlignment - cursor tag" $ do
    let
      dl0 = displayLinesWithAlignment TextAlignment_Right 10 0 1 (fromText "abc")
      dl1 = displayLinesWithAlignment TextAlignment_Right 10 0 1 empty
    _displayLines_spans dl0 `shouldBe` [[Span 0 "abc", Span 1 ""]]
    _displayLines_spans dl1 `shouldBe` [[Span 1 ""]]




  it "displayLines - cursorPos" $ do
    let
      dl0 = displayLinesWithAlignment TextAlignment_Left 10 () () (fromText "")
      dl1 = displayLinesWithAlignment TextAlignment_Left 10 () () (fromText "aoeu")
      dl2 = displayLinesWithAlignment TextAlignment_Left 10 () () (fromText "aoeu\n")
      dl3 = displayLinesWithAlignment TextAlignment_Left 10 () () (fromText "0123456789")
      dl4 = displayLinesWithAlignment TextAlignment_Left 10 () () (insertChar 'a' $ fromText "aoeu")
      dl5 = displayLinesWithAlignment TextAlignment_Left 10 () () (left $ insertChar 'a' $ fromText "aoeu")
      dl6 = displayLinesWithAlignment TextAlignment_Left 10 () () (deleteLeft $ insertChar 'a' $ fromText "aoeu")
      dl7 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "")

    _displayLines_cursorPos dl0 `shouldBe` (0,0)
    _displayLines_cursorPos dl1 `shouldBe` (4,0)
    _displayLines_cursorPos dl2 `shouldBe` (0,1)
    _displayLines_cursorPos dl3 `shouldBe` (0,1)
    _displayLines_cursorPos dl4 `shouldBe` (5,0)
    _displayLines_cursorPos dl5 `shouldBe` (4,0)
    _displayLines_cursorPos dl6 `shouldBe` (4,0)
    _displayLines_cursorPos dl7 `shouldBe` (10,0)
  it "displayLinesWithAlignment - spans" $ do
    let
      someText = top $ fromText "0123456789abcdefgh"
    -- outer span length should be invariant when changing TextAlignment and CursorPosition
    --print $ displayLinesWithAlignment TextAlignment_Left 5 () () someText
    forM_ [0..4] $ \x -> do
      forM_ [TextAlignment_Left, TextAlignment_Center, TextAlignment_Right] $ \ta -> do
        let t = rightN x $ someText
        length (_displayLines_spans $ (displayLinesWithAlignment ta 5 () () t)) `shouldBe` 4
        length (_displayLines_spans $ (displayLinesWithAlignment ta 10 () () t)) `shouldBe` 2
