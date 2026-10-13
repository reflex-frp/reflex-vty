{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.Widget.MouseSpec (spec) where

import qualified Graphics.Vty as V
import Test.Hspec

import Reflex.Vty.Widget.Input.Mouse

-- | Replay a list of vty events through 'stepDrag', keeping the previous drag
-- state when an event is irrelevant (mirroring how 'drag' holds its state).
-- Returns the 'Drag' emitted for each relevant event, in order.
runDrag :: V.Button -> [V.Event] -> [Drag]
runDrag btn = go Nothing
  where
    go _ [] = []
    go prev (e : es) = case stepDrag btn prev e of
      Just d -> d : go (Just d) es
      Nothing -> go prev es

down :: Int -> Int -> V.Button -> V.Event
down x y b = V.EvMouseDown x y b []

up :: Int -> Int -> Maybe V.Button -> V.Event
up x y b = V.EvMouseUp x y b

spec :: Spec
spec = describe "Reflex.Vty.Widget.Input.Mouse" $ do
  describe "stepDrag" $ do
    it "starts a drag on the first matching mouse-down" $
      stepDrag V.BLeft Nothing (down 3 4 V.BLeft)
        `shouldBe` Just (Drag (3, 4) (3, 4) V.BLeft [] DragStart)

    it "ignores a mouse-down for a different button" $
      stepDrag V.BLeft Nothing (down 3 4 V.BRight) `shouldBe` Nothing

    it "ignores non-mouse events" $
      stepDrag V.BLeft Nothing (V.EvKey (V.KChar 'a') []) `shouldBe` Nothing

    it "continues as Dragging on subsequent moves, preserving the origin" $ do
      let start = Drag (3, 4) (3, 4) V.BLeft [] DragStart
      stepDrag V.BLeft (Just start) (down 5 6 V.BLeft)
        `shouldBe` Just (Drag (3, 4) (5, 6) V.BLeft [] Dragging)

    it "ends the drag on mouse-up (button specified)" $ do
      let dragging = Drag (3, 4) (5, 6) V.BLeft [] Dragging
      stepDrag V.BLeft (Just dragging) (up 7 8 (Just V.BLeft))
        `shouldBe` Just (Drag (3, 4) (7, 8) V.BLeft [] DragEnd)

    it "ends the drag on mouse-up (button unspecified)" $ do
      let dragging = Drag (3, 4) (5, 6) V.BLeft [] Dragging
      stepDrag V.BLeft (Just dragging) (up 7 8 Nothing)
        `shouldBe` Just (Drag (3, 4) (7, 8) V.BLeft [] DragEnd)

    it "ignores a stray mouse-up after the drag already ended" $ do
      let ended = Drag (3, 4) (7, 8) V.BLeft [] DragEnd
      stepDrag V.BLeft (Just ended) (up 9 9 (Just V.BLeft)) `shouldBe` Nothing

    it "starts a fresh drag on a mouse-down after the previous one ended" $ do
      let ended = Drag (3, 4) (7, 8) V.BLeft [] DragEnd
      stepDrag V.BLeft (Just ended) (down 1 1 V.BLeft)
        `shouldBe` Just (Drag (1, 1) (1, 1) V.BLeft [] DragStart)

    it "produces Start -> Dragging -> End across a full drag" $ do
      let ds = runDrag V.BLeft [down 1 1 V.BLeft, down 2 2 V.BLeft, down 3 3 V.BLeft, up 3 3 (Just V.BLeft)]
      map _drag_state ds `shouldBe` [DragStart, Dragging, Dragging, DragEnd]
      map _drag_from ds `shouldBe` replicate 4 (1, 1)
      _drag_to (last ds) `shouldBe` (3, 3)

    it "tracks two consecutive drags, each with its own Start and End" $ do
      let evs =
            [ down 1 1 V.BLeft
            , up 1 1 (Just V.BLeft)
            , down 5 5 V.BLeft
            , down 6 6 V.BLeft
            , up 6 6 Nothing
            ]
      map _drag_state (runDrag V.BLeft evs)
        `shouldBe` [DragStart, DragEnd, DragStart, Dragging, DragEnd]
