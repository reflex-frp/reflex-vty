{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.Widget.ScrollSpec (spec) where

import Control.Monad (forM_)
import qualified Graphics.Vty as V
import Test.Hspec

import Reflex.Vty.Test.Snapshot
import Reflex.Vty.Widget.Scroll
  ( ScrollPos (..)
  , ScrollbarVisibility (..)
  , scrollByLines
  , scrollToLine
  , scrollbarImages
  )

-- | Specified normalization of an already-clamped target line.
expectedPosFromClamped :: Int -> Int -> Int -> ScrollPos
expectedPosFromClamped total height line
  | total <= height = ScrollPos_Top
  | line <= 0 = ScrollPos_Top
  | line + height >= total = ScrollPos_Bottom
  | otherwise = ScrollPos_Line line

-- | Specified normalization of a raw (possibly out-of-range) request.
expectedPos :: Int -> Int -> Int -> ScrollPos
expectedPos total height req = expectedPosFromClamped total height (max 0 req)

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

    it "produces a thumb and gutter for ScrollbarAlways" $ do
      let imgs = scrollbarImages ScrollbarAlways attr 10 50 0 20
      length imgs `shouldBe` 2 -- gutter + thumb
    it "produces only a thumb for ScrollbarThumbOnly" $ do
      let imgs = scrollbarImages ScrollbarThumbOnly attr 10 50 0 20
      length imgs `shouldBe` 1 -- thumb only
    it "produces only a thumb for ScrollbarWhileScrolling" $ do
      let imgs = scrollbarImages ScrollbarWhileScrolling attr 10 50 0 20
      length imgs `shouldBe` 1 -- thumb only
    it "places thumb at top when scrollLine is 0" $ do
      let imgs = scrollbarImages ScrollbarThumbOnly attr 10 50 0 20
          rows = imageText (V.vertCat imgs)
          -- Thumb should be at the top of the rightmost column
          firstChar [] = ' '
          firstChar (c : _) = c
      case rows of
        (r : _) -> last r `shouldBe` '█'
        [] -> expectationFailure "expected at least one rendered row"
      case rows of
        (r1 : _) -> firstChar r1 `shouldBe` ' '
        [] -> expectationFailure "expected at least one rendered row"

    it "places thumb lower when scrolled down" $ do
      let imgsAtTop = scrollbarImages ScrollbarThumbOnly attr 10 50 0 20
          imgsScrolled = scrollbarImages ScrollbarThumbOnly attr 10 50 20 20
          topRow = imageText (V.vertCat imgsAtTop)
          scrolledRows = imageText (V.vertCat imgsScrolled)
      case (topRow, scrolledRows) of
        ((top : _), (scrolled : _)) -> do
          last top `shouldBe` '█'
          -- When scrolled, the first row should not have the thumb
          last scrolled `shouldSatisfy` (/= '█')
        _ -> expectationFailure "expected rendered rows"

  describe "scrollToLine" $ do
    it "normalizes every small-domain request exactly" $ do
      -- Exhaustive over totalLines 0..6, height 0..4, request -2..8,
      -- against the specified semantics: content that fits is Top;
      -- nonpositive requests are Top; requests at/past the last page
      -- are Bottom; anything else is the exact line.
      forM_ [(0 :: Int) .. 6] $ \total ->
        forM_ [0 .. 4] $ \height ->
          forM_ [(-2) .. 8] $ \req ->
            scrollToLine total height req
              `shouldBe` expectedPos total height req

  describe "scrollByLines" $ do
    it "clamps and normalizes relative movement from every starting form" $ do
      forM_ [ScrollPos_Top, ScrollPos_Bottom, ScrollPos_Line 0, ScrollPos_Line 2, ScrollPos_Line 5] $ \sp ->
        forM_ [(0 :: Int) .. 6] $ \total ->
          forM_ [0 .. 4] $ \height ->
            forM_ [(-3), (-1), 0, 1, 4] $ \delta -> do
              -- The starting line is used literally; stored positions
              -- are always normalized in production, so Top is 0 and
              -- Bottom is the last page.
              let start = case sp of
                    ScrollPos_Top -> 0
                    ScrollPos_Bottom -> total - height
                    ScrollPos_Line n -> n
                  target = min total (max 0 (start + delta))
              scrollByLines sp total height delta
                `shouldBe` (if total <= height then ScrollPos_Top else expectedPosFromClamped total height target)

    it "thumb size is proportional to viewport/total ratio" $ do
      let attr' = V.defAttr
          -- vpHeight=10, totalLines=100 → thumbLen = max 1 (100/100) = 1
          imgs1 = scrollbarImages ScrollbarThumbOnly attr' 10 100 0 20
          -- vpHeight=10, totalLines=20 → thumbLen = max 1 (100/20) = 5
          imgs2 = scrollbarImages ScrollbarThumbOnly attr' 10 20 0 20
          heightOf [] = 0
          heightOf (img : _) = V.imageHeight img
      heightOf imgs1 `shouldBe` 1
      heightOf imgs2 `shouldBe` 5
