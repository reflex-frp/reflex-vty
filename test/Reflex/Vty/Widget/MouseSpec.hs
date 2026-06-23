{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.Widget.MouseSpec (spec) where

import qualified Graphics.Vty as V
import Test.Hspec

import Reflex.Vty.Widget.Input.Mouse

spec :: Spec
spec = describe "Reflex.Vty.Widget.Input.Mouse" $ do
  describe "Drag" $ do
    it "has correct field types" $ do
      let d = Drag (0, 0) (1, 1) V.BLeft [] False
      _drag_from d `shouldBe` (0, 0)
      _drag_to d `shouldBe` (1, 1)
      _drag_button d `shouldBe` V.BLeft
      _drag_end d `shouldBe` False

  describe "MouseDown" $ do
    it "stores button, coordinates, and modifiers" $ do
      let md = MouseDown V.BLeft (5, 10) [V.MShift]
      _mouseDown_button md `shouldBe` V.BLeft
      _mouseDown_coordinates md `shouldBe` (5, 10)
      _mouseDown_modifiers md `shouldBe` [V.MShift]

  describe "MouseUp" $ do
    it "stores optional button and coordinates" $ do
      let mu = MouseUp (Just V.BLeft) (3, 7)
      _mouseUp_button mu `shouldBe` Just V.BLeft
      _mouseUp_coordinates mu `shouldBe` (3, 7)

  describe "ScrollDirection" $ do
    it "has up and down constructors" $ do
      ScrollDirection_Up `shouldNotBe` ScrollDirection_Down
