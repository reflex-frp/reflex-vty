{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.Widget.BoxSpec (spec) where

import Data.Text.Zipper (TextAlignment (..))
import qualified Data.Text as T
import Test.Hspec

import Reflex.Vty.Widget.Box (alignText, centerText)

spec :: Spec
spec = describe "Reflex.Vty.Widget.Box" $ do
  describe "alignText" $ do
    it "left-aligns by padding on the right" $ do
      alignText TextAlignment_Left "hi" '-' 5 `shouldBe` "hi---"

    it "right-aligns by padding on the left" $ do
      alignText TextAlignment_Right "hi" '-' 5 `shouldBe` "---hi"

    it "center-aligns with left bias for odd slack" $ do
      alignText TextAlignment_Center "hi" '-' 5 `shouldBe` "--hi-"

    it "center-aligns evenly for even slack" $ do
      alignText TextAlignment_Center "hi" '-' 6 `shouldBe` "--hi--"

    it "returns text unchanged when it fills the width" $ do
      alignText TextAlignment_Left "hello" '-' 3 `shouldBe` "hello"

    it "returns text unchanged when it exceeds the width" $ do
      alignText TextAlignment_Center "hello" '-' 2 `shouldBe` "hello"

    it "measures wide characters correctly" $ do
      alignText TextAlignment_Left "文" '-' 4 `shouldBe` "文--"

    it "handles empty text" $ do
      alignText TextAlignment_Center "" '-' 3 `shouldBe` "---"

  describe "centerText" $ do
    it "is a specialization of alignText" $ do
      centerText "hi" '-' 5 `shouldBe` alignText TextAlignment_Center "hi" '-' 5
