-- |
-- Module: Reflex.Vty.Canvas
-- Description: Per-cell compositing with transparency
--
-- A 'Canvas' is a 2D grid of @Maybe (Char, Attr)@ cells where 'Nothing'
-- means transparent. This allows compositing visual layers with
-- per-cell transparency — something 'V.Image' cannot do.
--
-- Typical usage: capture a child widget's images via 'captureImages',
-- convert to a 'Canvas' with 'imageToCanvas', composite overlay
-- 'Canvas'es on top with 'stack', then convert back to an 'Image' with
-- 'canvasToImage' and emit via 'tellImages'.
module Reflex.Vty.Canvas
  ( Canvas (..)
  , canvasCellAt
  , blankCanvas
  , placeCanvas
  , translate
  , stack
  , imageToCanvas
  , canvasToImage
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Graphics.Text.Width (wcwidth)
import qualified Graphics.Vty as V
import Graphics.Vty.Image.Internal (Image (BGFill, Crop, EmptyImage, HorizJoin, HorizText, VertJoin))

-- | A 2D grid of cells. Cells not in the 'Map' are transparent
-- ('Nothing'). The 'Map' is keyed by @(x, y)@ where @(0,0)@ is the
-- top-left corner.
data Canvas = Canvas
  { canvasWidth :: !Int
  , canvasHeight :: !Int
  , canvasCells :: !(Map (Int, Int) (Char, V.Attr))
  }
  deriving (Eq, Show)

-- | A fully transparent canvas of the given dimensions.
blankCanvas :: Int -> Int -> Canvas
blankCanvas w h = Canvas w h Map.empty

-- | Look up the cell at @(x, y)@. Returns 'Nothing' for transparent cells.
canvasCellAt :: Int -> Int -> Canvas -> Maybe (Char, V.Attr)
canvasCellAt x y (Canvas _ _ cells) = Map.lookup (x, y) cells

-- | Place a source canvas onto a destination at offset @(dx, dy)@.
-- Transparent cells in the source do not overwrite the destination.
-- Cells outside the destination bounds are clipped.
placeCanvas :: Int -> Int -> Canvas -> Canvas -> Canvas
placeCanvas dx dy src dst =
  dst {canvasCells = foldl' insert (canvasCells dst) visibleCells}
  where
    insert m (pos, cell) = Map.insert pos cell m
    visibleCells =
      [ ((dx + sx, dy + sy), cell)
      | ((sx, sy), cell) <- Map.toList (canvasCells src)
      , dx + sx >= 0
      , dy + sy >= 0
      , dx + sx < canvasWidth dst
      , dy + sy < canvasHeight dst
      ]

-- | Shift a canvas by @(dx, dy)@. Dimensions are preserved; cells that
-- move outside bounds are lost, and newly empty cells are transparent.
translate :: Int -> Int -> Canvas -> Canvas
translate dx dy src =
  src {canvasCells = Map.mapKeys (\(x, y) -> (x + dx, y + dy)) clipped}
  where
    clipped = Map.filterWithKey keep (canvasCells src)
    keep (x, y) _ =
      x + dx >= 0
        && y + dy >= 0
        && x + dx < canvasWidth src
        && y + dy < canvasHeight src

-- | Stack canvases in z-order (last = topmost). All canvases must have
-- the same dimensions; later canvases overlay earlier ones.
stack :: [Canvas] -> Canvas
stack [] = blankCanvas 0 0
stack (c : cs) = foldl' (\dst src -> placeCanvas 0 0 src dst) c cs

----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

-- | An attr with all fields 'KeepCurrent', used for transparent cells
-- when converting to an 'Image'.
transparentAttr :: V.Attr
transparentAttr =
  V.Attr
    { V.attrStyle = V.KeepCurrent
    , V.attrForeColor = V.KeepCurrent
    , V.attrBackColor = V.KeepCurrent
    , V.attrURL = V.KeepCurrent
    }

-- | Convert a vty 'Image' to an opaque 'Canvas'. Every cell in the
-- image becomes a concrete @(Char, Attr)@ in the canvas; there are no
-- transparent cells.
imageToCanvas :: Image -> Canvas
imageToCanvas img =
  Canvas
    { canvasWidth = V.imageWidth img
    , canvasHeight = V.imageHeight img
    , canvasCells = walkImage img 0 0 Map.empty
    }

-- | Convert a 'Canvas' to a vty 'Image'. Transparent cells (not in the
-- 'Map') are rendered as spaces with 'transparentAttr' (KeepCurrent),
-- allowing underlying vty layers to show through.
canvasToImage :: Canvas -> Image
canvasToImage (Canvas w h cells)
  | w <= 0 || h <= 0 = EmptyImage
  | otherwise = V.vertCat (map renderRow [0 .. h - 1])
  where
    renderRow y = V.horizCat (map (uncurry V.text') (rowRuns y))
    rowRuns y = mergeRuns [0 .. w - 1]
      where
        cellAt x = Map.findWithDefault (' ', transparentAttr) (x, y) cells
        mergeRuns [] = []
        mergeRuns (x : xs) = go (cellAt x) [fst (cellAt x)] xs
        go (_, attr) chars (x : xs)
          | attr == snd (cellAt x) = go (cellAt x) (fst (cellAt x) : chars) xs
          | otherwise = (attr, T.pack (reverse chars)) : mergeRuns (x : xs)
        go (_, attr) chars [] = [(attr, T.pack (reverse chars))]

----------------------------------------------------------------------------
-- Internal: Image tree walker
----------------------------------------------------------------------------

type CellMap = Map (Int, Int) (Char, V.Attr)

walkImage :: Image -> Int -> Int -> CellMap -> CellMap
walkImage img x y acc =
  case img of
    HorizText attr displayText _ _ ->
      placeText attr (TL.unpack displayText) x y acc
    HorizJoin partLeft partRight _ _ ->
      let acc' = walkImage partLeft x y acc
      in walkImage partRight (x + V.imageWidth partLeft) y acc'
    VertJoin partTop partBottom _ _ ->
      let acc' = walkImage partTop x y acc
      in walkImage partBottom x (y + V.imageHeight partTop) acc'
    BGFill outputWidth outputHeight ->
      foldl'
        (\m (dx, dy) -> Map.insertWith (\_ old -> old) (x + dx, y + dy) (' ', V.defAttr) m)
        acc
        [(dx, dy) | dx <- [0 .. outputWidth - 1], dy <- [0 .. outputHeight - 1]]
    Crop croppedImage leftSkip topSkip outputWidth outputHeight ->
      let innerCells = walkImage croppedImage 0 0 Map.empty
          visible =
            [ ((x + kx - leftSkip, y + ky - topSkip), cell)
            | ((kx, ky), cell) <- Map.toList innerCells
            , kx >= leftSkip
            , kx < leftSkip + outputWidth
            , ky >= topSkip
            , ky < topSkip + outputHeight
            ]
      in foldl' (\m (pos, cell) -> Map.insert pos cell m) acc visible
    EmptyImage -> acc

placeText :: V.Attr -> String -> Int -> Int -> CellMap -> CellMap
placeText attr chars x y =
  go chars 0
  where
    go [] _ acc = acc
    go (c : cs) dx acc =
      let w = wcwidth c
      in if w <= 0
           then go cs dx acc
           else go cs (dx + w) (Map.insert (x + dx, y) (c, attr) acc)
