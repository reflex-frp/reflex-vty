{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.Widget.ScrollSpec (spec) where

import qualified Graphics.Vty as V
import Test.Hspec

import Reflex.Vty.Widget.Scroll
  ( ScrollbarVisibility (..)
  , scrollbarImages
  )
import Reflex.Vty.Test.Snapshot

spec :: Spec
spec = describe "Reflex.Vty.Widget.Scroll" $ do
  describe "scrollbarImages" $ do
    let attr = V.defAttr

    it "returns empty when hidden" $ do
      scrollbarImages ScrollbarHidden attr 10 50 0 20 `shouldBe` []

    it "returns empty when content fits viewport" $ do
      scrollbarImages ScrollbarAlways attr 10 5 0 20 `shouldBe` []

    it "returns empty when viewport too narrow" $ do
      scrollbarImages ScrollbarAlways attr 10 50 0 1 `shouldBe` []

    it "produces a thumb for ScrollbarAlways" $ do
      let imgs = scrollbarImages ScrollbarAlways attr 10 50 0 20
      length imgs `shouldBe` 2  -- gutter + thumb

    it "produces only a thumb for ScrollbarThumbOnly" $ do
      let imgs = scrollbarImages ScrollbarThumbOnly attr 10 50 0 20
      length imgs `shouldBe` 1  -- thumb only

    it "produces only a thumb for ScrollbarWhileScrolling" $ do
      let imgs = scrollbarImages ScrollbarWhileScrolling attr 10 50 0 20
      length imgs `shouldBe` 1  -- thumb only

    it "places thumb at top when scrollLine is 0" $ do
      let imgs = scrollbarImages ScrollbarThumbOnly attr 10 50 0 20
          grid = imageToGrid $ V.vertCat imgs
          -- Thumb should be at the top of the rightmost column
          (row0, _) = case grid of
            (r : _) -> (map cellChar r, 0)
            [] -> ("", 0)
      last row0 `shouldBe` '█'

    it "places thumb lower when scrolled down" $ do
      let imgsAtTop = scrollbarImages ScrollbarThumbOnly attr 10 50 0 20
          imgsScrolled = scrollbarImages ScrollbarThumbOnly attr 10 50 20 20
          topGrid = imageToGrid $ V.vertCat imgsAtTop
          scrolledGrid = imageToGrid $ V.vertCat imgsScrolled
          topChar (row : _) = last (map cellChar row)
          topChar [] = ' '
      -- The thumb at position 0 should differ from thumb at position 20
      topChar topGrid `shouldBe` '█'
      -- When scrolled, the first row should not have the thumb
      topChar scrolledGrid `shouldSatisfy` (/= '█')

    it "thumb size is proportional to viewport/total ratio" $ do
      let attr = V.defAttr
          -- vpHeight=10, totalLines=100 → thumbLen = max 1 (100/100) = 1
          imgs1 = scrollbarImages ScrollbarThumbOnly attr 10 100 0 20
          -- vpHeight=10, totalLines=20 → thumbLen = max 1 (100/20) = 5
          imgs2 = scrollbarImages ScrollbarThumbOnly attr 10 20 0 20
          heightOf [] = 0
          heightOf (img : _) = V.imageHeight img
      heightOf imgs1 `shouldBe` 1
      heightOf imgs2 `shouldBe` 5
