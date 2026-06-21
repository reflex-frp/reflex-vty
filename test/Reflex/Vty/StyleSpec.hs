{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.StyleSpec (spec) where

import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as V.Color
import qualified Graphics.Vty.Image as V.Image
import Test.Hspec

import Reflex.Vty.Style

spec :: Spec
spec = describe "Reflex.Vty.Style" $ do
  describe "def" $ do
    it "has no foreground" $
      _style_foreground def `shouldBe` Nothing
    it "has no border" $
      _style_border def `shouldBe` Nothing
    it "has zero padding" $
      _style_padding def `shouldBe` Padding 0 0 0 0

  describe "inherit" $ do
    let parent = withForeground red . withBold . withWidth 10 $ def
    it "keeps the child's foreground when set" $ do
      let child = withForeground blue def
      _style_foreground (inherit parent child) `shouldBe` Just blue
    it "inherits the parent's foreground when the child's is Nothing" $ do
      let child = def
      _style_foreground (inherit parent child) `shouldBe` Just red
    it "keeps the child's width when set" $ do
      let child = withWidth 5 def
      _style_width (inherit parent child) `shouldBe` Just 5
    it "inherits the parent's width when the child's is Nothing" $ do
      let child = def
      _style_width (inherit parent child) `shouldBe` Just 10
    it "inherits the parent's bold flag when the child's is Nothing" $ do
      let child = def
      _style_bold (inherit parent child) `shouldBe` Just True

  describe "applyAttr" $ do
    it "applies the foreground color" $ do
      let s = withForeground red def
      V.attrForeColor (applyAttr s V.defAttr) `shouldBe` V.SetTo red
    it "applies the background color" $ do
      let s = withBackground blue def
      V.attrBackColor (applyAttr s V.defAttr) `shouldBe` V.SetTo blue
    it "applies bold when Just True" $ do
      let s = withBold def
      V.hasStyle (V.styleMask (applyAttr s V.defAttr)) V.bold `shouldBe` True
    it "does not apply bold when Nothing" $ do
      V.hasStyle (V.styleMask (applyAttr def V.defAttr)) V.bold `shouldBe` False
    it "applies underline for UnderlineSingle" $ do
      let s = withUnderline UnderlineSingle def
      V.hasStyle (V.styleMask (applyAttr s V.defAttr)) V.underline `shouldBe` True
    it "does not apply underline for UnderlineNone" $ do
      let s = withUnderline UnderlineNone def
      V.hasStyle (V.styleMask (applyAttr s V.defAttr)) V.underline `shouldBe` False
    it "applies the hyperlink URL" $ do
      let s = withHyperlink "https://example.com" def
      V.attrURL (applyAttr s V.defAttr) `shouldBe` V.SetTo "https://example.com"
    it "leaves the base attr unchanged for def" $ do
      applyAttr def V.defAttr `shouldBe` V.defAttr

  describe "border style presets" $ do
    it "singleBorder has all sides and corners" $ do
      all
        (isJust . ($ singleBorder))
        [ _border_top
        , _border_bottom
        , _border_left
        , _border_right
        , _border_topLeft
        , _border_topRight
        , _border_bottomLeft
        , _border_bottomRight
        ]
        `shouldBe` True
    it "noBorder has no sides or corners" $ do
      any
        (isJust . ($ noBorder))
        [ _border_top
        , _border_bottom
        , _border_left
        , _border_right
        , _border_topLeft
        , _border_topRight
        , _border_bottomLeft
        , _border_bottomRight
        ]
        `shouldBe` False
    it "markdownBorder has sides but no corners" $ do
      let b = markdownBorder
      _border_top b `shouldSatisfy` isJust
      _border_topLeft b `shouldBe` Nothing

  describe "render" $ do
    it "produces a non-empty image for plain text" $ do
      let img = render def "hello"
      V.Image.imageWidth img `shouldSatisfy` (> 0)
      V.Image.imageHeight img `shouldBe` 1
    it "respects withWidth by padding" $ do
      let img = render (withWidth 10 def) "hi"
      V.Image.imageWidth img `shouldBe` 10
    it "respects withHeight by padding" $ do
      let img = render (withHeight 3 def) "hi"
      V.Image.imageHeight img `shouldBe` 3
    it "respects withMaxWidth by clipping" $ do
      let img = render (withMaxWidth 3 def) "hello"
      V.Image.imageWidth img `shouldBe` 3
    it "respects withMaxHeight by clipping" $ do
      let img = render (withMaxHeight 1 def) "hello"
      V.Image.imageHeight img `shouldBe` 1
    it "adds padding around content" $ do
      let s = withPadding 1 1 1 1 def
          img = render s "hi"
      V.Image.imageWidth img `shouldBe` 4
      V.Image.imageHeight img `shouldBe` 3
    it "adds a border around content" $ do
      let s = withBorder singleBorder def
          img = render s "hi"
      V.Image.imageWidth img `shouldBe` 4
      V.Image.imageHeight img `shouldBe` 3
    it "adds margin outside content" $ do
      let s = withMargin 1 1 1 1 def
          img = render s "hi"
      V.Image.imageWidth img `shouldBe` 4
      V.Image.imageHeight img `shouldBe` 3
    it "renders multi-line text with correct height" $ do
      let img = render def "hello\nworld"
      V.Image.imageHeight img `shouldBe` 2
    it "renders multi-line text width as longest line" $ do
      let img = render def "hello\nhi"
      V.Image.imageWidth img `shouldBe` 5
    it "renders wide characters at correct width" $ do
      let img = render def "文"
      V.Image.imageWidth img `shouldBe` 2

  describe "measure" $ do
    it "matches render's width for plain text" $ do
      let s = def
      fst (measure s "hello") `shouldBe` V.Image.imageWidth (render s "hello")
    it "accounts for padding" $ do
      let s = withPadding 2 3 2 3 def
      fst (measure s "hi") `shouldBe` 2 + 3 + 3
    it "accounts for border" $ do
      let s = withBorder singleBorder def
      fst (measure s "hi") `shouldBe` 2 + 2
    it "accounts for margin" $ do
      let s = withMargin 1 1 1 1 def
      fst (measure s "hi") `shouldBe` 2 + 2
    it "measures multi-line text with correct height" $ do
      snd (measure def "hello\nworld") `shouldBe` 2
    it "measures multi-line text with correct width" $ do
      fst (measure def "hello\nhi") `shouldBe` 5
    it "measures multi-line text with matching render dimensions" $ do
      let s = withBorder singleBorder def
          (w, h) = measure s "hello\nworld"
          img = render s "hello\nworld"
      w `shouldBe` V.Image.imageWidth img
      h `shouldBe` V.Image.imageHeight img
    it "measures wide characters at display width" $ do
      fst (measure def "文") `shouldBe` 2
    it "measures mixed wide and narrow characters" $ do
      fst (measure def "a文b") `shouldBe` 4
    it "measures empty content as zero width, one line" $ do
      measure def "" `shouldBe` (0, 1)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
