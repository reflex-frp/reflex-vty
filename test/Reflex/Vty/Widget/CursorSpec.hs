module Reflex.Vty.Widget.CursorSpec (spec) where

import qualified Graphics.Vty as V
import Test.Hspec

import Reflex.Vty.Host
import Reflex.Vty.Widget

spec :: Spec
spec = describe "Reflex.Vty.Widget cursor" $ do
  describe "defaultCursorState" $
    it "starts hidden with terminal-default style" $
      defaultCursorState `shouldBe` CursorState CursorHidden CursorStyleDefault (0, 0)

  describe "translateCursorState" $ do
    it "translates visible viewport-relative positions to absolute positions" $
      translateCursorState
        (Region 10 20 5 3)
        (CursorState CursorVisible CursorStyleBar (2, 1))
        `shouldBe` CursorState CursorVisible CursorStyleBar (12, 21)

    it "hides visible cursors outside the viewport" $
      translateCursorState
        (Region 10 20 5 3)
        (CursorState CursorVisible CursorStyleBlock (5, 1))
        `shouldBe` CursorState CursorHidden CursorStyleBlock (15, 21)

    it "translates hidden cursor positions without changing visibility" $
      translateCursorState
        (Region 10 20 5 3)
        (CursorState CursorHidden CursorStyleUnderline (7, 4))
        `shouldBe` CursorState CursorHidden CursorStyleUnderline (17, 24)

  describe "cursorStateToVtyCursor" $ do
    it "maps hidden cursor state to NoCursor" $
      cursorStateToVtyCursor (CursorState CursorHidden CursorStyleBlock (2, 3))
        `shouldBe` V.NoCursor

    it "maps visible cursor state to a vty cursor position" $
      cursorStateToVtyCursor (CursorState CursorVisible CursorStyleBlock (2, 3))
        `shouldBe` V.Cursor 2 3

  describe "ScreenMode" $ do
    it "has two constructors" $
      [ScreenNormal ..] `shouldBe` [ScreenNormal, ScreenAlternate]

    it "ScreenNormal /= ScreenAlternate" $
      ScreenNormal `shouldNotBe` ScreenAlternate
