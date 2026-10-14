{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

-- |
--   Description: An interactive drawing surface over 'Reflex.Vty.Canvas',
--   exercising glyphs, transparency, and clipping that flat images hide.
module Example.CanvasDemo (canvasDemo) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Graphics.Text.Width (wcwidth)
import qualified Graphics.Vty as V
import Reflex

import Example.Common
import Reflex.Vty
import Reflex.Vty.Canvas

-- | A small canvas board driven by the arrow keys. A two-column glyph and a
-- combining-mark glyph sit on a checkerboard that shows through transparency.
-- A two-cell highlight moves over them, and the board's right edge clips a
-- wide glyph to Vty's ellipsis form.
canvasDemo :: Demo t m => m (Event t ())
canvasDemo = do
  inp <- input
  let moves = fforMaybe inp $ \case
        V.EvKey V.KLeft [] -> Just (-2, 0)
        V.EvKey V.KRight [] -> Just (2, 0)
        V.EvKey V.KUp [] -> Just (0, -1)
        V.EvKey V.KDown [] -> Just (0, 1)
        _ -> Nothing
      clamp v lo hi = max lo (min hi v)
  pos <-
    foldDyn
      (\(dx, dy) (x, y) -> (clamp (x + dx) 0 (boardW - 2), clamp (y + dy) 0 (boardH - 1)))
      (2, 2)
      moves
  dw <- displayWidth
  dh <- displayHeight
  tellImages $ current $ ffor ((,) <$> pos <*> ((,) <$> dw <*> dh)) $ \((x, y), (w, h)) ->
    [ canvasToImage (board (min w boardW) (min h boardH) x y)
    , V.char V.defAttr ' '
    , V.text' (V.withForeColor V.defAttr dim) (T.pack hint)
    ]
  quit

-- Board dimensions: wide enough that the glyph layer's last wide glyph
-- straddles the right edge and clips.
boardW, boardH :: Int
boardW = 22
boardH = 6

hint :: String
hint = "  arrows: move · q: quit · 漢 is wide; á is one glyph with a combining mark"

-- | The demo board: transparent base, the glyph layer below, and the moving
-- highlight on top. The glyph layer is drawn one column wider and then
-- translated left, so its last wide glyph crosses the board's right edge and
-- clips to an ellipsis.
board :: Int -> Int -> Int -> Int -> Canvas
board w h hx hy =
  stack
    [ blankCanvas w h
    , translate (-1) 0 (glyphLayer (w + 1) h)
    , highlightLayer w h hx hy
    ]

-- | The fixed layer: a checkerboard of dots (transparency lets the base show
-- between them), a wide glyph, and a combining-mark glyph whose mark carries a
-- different attribute than its base.
glyphLayer :: Int -> Int -> Canvas
glyphLayer w h =
  Canvas w h $
    Map.unions
      [checker, wide, marked]
  where
    -- Map.unions is left-biased, so glyphs must come first and the
    -- checkerboard last; it only fills columns the glyphs leave free.
    checker =
      Map.fromList
        [ ((x, y), cellAttr (if even (x + y) then dimA else faintA) "·")
        | y <- [0 .. h - 1]
        , x <- [0 .. w - 1]
        , not (Map.member (x, y) glyphCells)
        ]
    glyphCells = Map.union wide marked
    -- At the layer's last column; after the left translate its second column
    -- falls outside the board and clips to an ellipsis.
    wide = Map.fromList [((w - 1, 2), plainCell "漢")]
    marked = Map.fromList [((13, 2), markedCell)]

-- | The moving overlay: an opaque left bracket and a full block. Placing it
-- over the wide glyph clips the wide glyph with Vty's ellipsis policy, and
-- over the dots shows transparency gaps around the overlay glyphs.
highlightLayer :: Int -> Int -> Int -> Int -> Canvas
highlightLayer w h hx hy =
  Canvas w h $
    Map.fromList
      [ ((hx, hy), plainCell "[")
      , ((hx + 1, hy), plainCell "\x2588")
      ]

-- Attributes and cell helpers ------------------------------------------------

plainA, dimA, faintA, markA :: V.Attr
plainA = V.defAttr
dimA = V.withForeColor V.defAttr (rgbColor 120 120 140)
faintA = V.withForeColor V.defAttr (rgbColor 70 70 85)
markA = V.withForeColor V.defAttr (rgbColor 255 120 200)

-- | One attributed run as a cell.
cellAttr :: V.Attr -> T.Text -> CanvasCell
cellAttr a t = CanvasCell width (pure (a, t))
  where
    width = sum [wcwidth ch | ch <- T.unpack t, wcwidth ch > 0]

-- | A base glyph plus a zero-width combining mark in its own attribute run.
markedCell :: CanvasCell
markedCell = CanvasCell 1 ((plainA, "a") :| [(markA, "\x0301")])

plainCell :: T.Text -> CanvasCell
plainCell = cellAttr plainA
