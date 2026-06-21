{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.Test.SnapshotSpec (spec) where

import Data.List (isInfixOf)
import qualified Graphics.Vty as V
import Test.Hspec

import Reflex.Vty.Style
  ( Color
  , Style
  , def
  , render
  , singleBorder
  , withBorder
  , withForeground
  )
import qualified Reflex.Vty.Style as Style (red)
import Reflex.Vty.Test.Snapshot

spec :: Spec
spec = describe "Reflex.Vty.Test.Snapshot" $ do
  describe "imageToGrid" $ do
    it "renders plain text as a single row" $ do
      let img = render def "hello"
          grid = imageToGrid img
      length grid `shouldBe` 1
      map cellChar (head grid) `shouldBe` "hello"

    it "renders multi-line text with correct rows" $ do
      let img = render def "ab\ncd"
          grid = imageToGrid img
      length grid `shouldBe` 2
      map cellChar (grid !! 0) `shouldBe` "ab"
      map cellChar (grid !! 1) `shouldBe` "cd"

    it "preserves attributes from the style" $ do
      let img = render (withForeground Style.red def) "X"
          grid = imageToGrid img
          attr = cellAttr (head (head grid))
      V.attrForeColor attr `shouldBe` V.SetTo Style.red

    it "renders borders as box-drawing characters" $ do
      let img = render (withBorder singleBorder def) "hi"
          grid = imageToGrid img
          topRow = map cellChar (head grid)
      head topRow `shouldBe` '┌'
      last topRow `shouldBe` '┐'

  describe "imageText" $ do
    it "extracts text content from image" $ do
      let img = render def "hello"
      imageText img `shouldBe` ["hello"]

    it "handles multi-line content" $ do
      let img = render def "foo\nbar"
      imageText img `shouldBe` ["foo", "bar"]

  describe "gridEquals" $ do
    it "returns Nothing for identical grids" $ do
      let grid = imageToGrid $ render def "hi"
      gridEquals grid grid `shouldBe` Nothing

    it "reports mismatched cells" $ do
      let g1 = imageToGrid $ render def "hi"
          g2 = imageToGrid $ render def "ho"
      gridEquals g1 g2 `shouldSatisfy` (/= Nothing)

  describe "renderGrid" $ do
    it "includes text representation" $ do
      let txt = renderGrid $ imageToGrid $ render def "X"
      txt `shouldSatisfy` ("X" `isInfixOf`)
