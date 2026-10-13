-- |
-- Module: Reflex.Vty.Canvas
-- Description: Per-glyph compositing with transparency
--
-- A 'Canvas' is a 2D grid keyed by glyph-start cells. Absent keys
-- mean transparent. Each 'CanvasCell' holds one positive-width base
-- character and its zero-width text. This records wide glyphs and
-- combining marks exactly. A wide glyph occupies its continuation
-- column without a second map entry. Combining marks keep their own
-- attribute runs inside the covered glyph.
--
-- 'imageToCanvas' decodes the spans that Vty rendered.
-- 'canvasToImage' emits transparent gaps as 'V.backgroundFill'. An
-- underlying image then shows through. Partial retention of a wide
-- glyph follows Vty's ellipsis policy. The retained half becomes a
-- width-one @…@. Right clipping drops a trailing combining suffix.
-- Left clipping keeps it.
--
-- Typical usage: capture a child widget's images via 'captureImages',
-- convert to a 'Canvas' with 'imageToCanvas', composite overlay
-- 'Canvas'es on top with 'stack', then convert back to an 'Image' with
-- 'canvasToImage' and emit via 'tellImages'.
module Reflex.Vty.Canvas
  ( Canvas (..)
  , CanvasCell (..)
  , canvasCellAt
  , blankCanvas
  , placeCanvas
  , translate
  , stack
  , imageToCanvas
  , canvasToImage
  ) where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vec
import Graphics.Text.Width (wcwidth)
import qualified Graphics.Vty as V
import Graphics.Vty.DisplayAttributes (fixDisplayAttr)
import qualified Graphics.Vty.PictureToSpans as PTS
import Graphics.Vty.Span (SpanOp (RowEnd, Skip, TextSpan))

-- | A 2D grid of glyph cells. The 'Map' is keyed by @(x, y)@ where
-- @(0,0)@ is the top-left corner and @x@ is the glyph's /start/
-- column. Keys absent from the map are transparent. Valid canvases
-- have nonoverlapping, in-bounds glyph intervals whose stored widths
-- match their text. 'imageToCanvas' and the combinators below
-- preserve this invariant.
data Canvas = Canvas
  { canvasWidth :: !Int
  , canvasHeight :: !Int
  , canvasCells :: !(Map (Int, Int) CanvasCell)
  }
  deriving (Eq, Show)

-- | One glyph: its total column width on screen and its attributed
-- text runs in order. The runs are the zero-width prefix runs, the
-- single positive-width base run, and the zero-width suffix runs.
-- Consecutive runs always differ in attribute. 'canvasCellColumns' is
-- the Vty width of the payload, normally 1 or 2.
data CanvasCell = CanvasCell
  { canvasCellColumns :: !Int
  , canvasCellRuns :: !(NonEmpty (V.Attr, T.Text))
  }
  deriving (Eq, Show)

-- | A fully transparent canvas of the given dimensions.
blankCanvas :: Int -> Int -> Canvas
blankCanvas w h = Canvas w h Map.empty

-- | Look up the glyph covering @(x, y)@. This is either the glyph
-- starting at @x@ or the glyph whose continuation column @x@ is.
-- Returns 'Nothing' for transparent and out-of-bounds coordinates.
canvasCellAt :: Int -> Int -> Canvas -> Maybe CanvasCell
canvasCellAt x y (Canvas w h cells)
  | x < 0 || y < 0 || x >= w || y >= h = Nothing
  | otherwise = case Map.lookup (x, y) cells of
      Just c -> Just c
      Nothing -> do
        prev <- Map.lookup (x - 1, y) cells
        if x - 1 + canvasCellColumns prev > x then Just prev else Nothing

----------------------------------------------------------------------------
-- Glyph anatomy and clipping
----------------------------------------------------------------------------

-- | A glyph split around its single positive-width base character: the
-- zero-width prefix runs, the base run's attribute, and the zero-width
-- suffix runs.
data GlyphParts = GlyphParts
  { glyphPrefix :: [(V.Attr, T.Text)]
  , glyphBaseAttr :: V.Attr
  , glyphSuffix :: [(V.Attr, T.Text)]
  }

splitGlyph :: CanvasCell -> GlyphParts
splitGlyph (CanvasCell _ glyphRuns) = go (NE.toList glyphRuns) []
  where
    go [] acc = GlyphParts (dropNull acc) V.defAttr []
    go ((attr, txt) : rest) acc =
      case T.findIndex (\c -> wcwidth c > 0) txt of
        Nothing -> go rest (acc ++ [(attr, txt)])
        Just i ->
          let (before, fromBase) = T.splitAt i txt
              afterTxt = T.drop 1 fromBase
          in GlyphParts
               { glyphPrefix = dropNull (acc ++ [(attr, before)])
               , glyphBaseAttr = attr
               , glyphSuffix = dropNull ([(attr, afterTxt)] ++ rest)
               }

dropNull :: [(a, T.Text)] -> [(a, T.Text)]
dropNull = filter (not . T.null . snd)

-- | Build a cell from runs, coalescing adjacent equal-attribute runs
-- and computing the column width from the text itself.
cellFromRuns :: [(V.Attr, T.Text)] -> CanvasCell
cellFromRuns runs0 =
  let glyphRuns = coalesce (dropNull runs0)
      width = sum [w | (_, txt) <- glyphRuns, ch <- T.unpack txt, let w = wcwidth ch, w > 0]
  in CanvasCell width (fromMaybe ((V.defAttr, "\x2026") :| []) (NE.nonEmpty glyphRuns))
  where
    coalesce = List.foldl' step []
    step [] r = [r]
    step acc r@(attr, txt)
      | attr == fst (last acc) = init acc ++ [(attr, snd (last acc) <> txt)]
      | otherwise = acc ++ [r]

-- | The clipped cell that keeps the /left/ part of a glyph. It is a
-- width-one @…@ with the base attribute and any zero-width prefix.
-- The zero-width suffix is dropped, as in Vty's right-edge clipping.
clipLeftPart :: GlyphParts -> CanvasCell
clipLeftPart parts = cellFromRuns (glyphPrefix parts ++ [(glyphBaseAttr parts, "\x2026")])

-- | The clipped cell that keeps the /right/ part. It is a width-one
-- @…@ with the base attribute, followed by the zero-width suffix. This
-- matches Vty's left-edge clipping.
clipRightPart :: GlyphParts -> CanvasCell
clipRightPart parts = cellFromRuns ((glyphBaseAttr parts, "\x2026") : glyphSuffix parts)

-- | Clip the glyph starting at column @x@ to the window @[lo, hi)@.
-- Wholly outside yields nothing. A partially retained glyph becomes
-- its clipped half.
clipInterval :: Int -> Int -> Int -> CanvasCell -> Maybe (Int, CanvasCell)
clipInterval lo hi x c
  | end <= lo || x >= hi = Nothing
  | x >= lo && end <= hi = Just (x, c)
  | x < lo = Just (lo, clipRightPart (splitGlyph c))
  | otherwise = Just (x, clipLeftPart (splitGlyph c))
  where
    end = x + canvasCellColumns c

-- | The columns occupied by opaque glyphs.
coveredColumns :: Map Int CanvasCell -> [Int]
coveredColumns =
  concatMap (\(x, c) -> [x .. x + canvasCellColumns c - 1]) . Map.toList

-- | Clip a destination glyph against the overwritten columns. This
-- can split it into several retained cells. A wholly covered glyph
-- disappears. A partially covered glyph is clipped to each of its
-- remaining column runs.
clipAgainst :: [Int] -> Int -> CanvasCell -> [(Int, CanvasCell)]
clipAgainst covered x c =
  case runs (filter (`notElem` covered) [x .. end - 1]) of
    [(s, l)] | (s, l) == (x, canvasCellColumns c) -> [(x, c)]
    rs -> map mk rs
  where
    end = x + canvasCellColumns c
    parts = splitGlyph c
    mk (s, _)
      | s == x = (s, clipLeftPart parts)
      | otherwise = (s, clipRightPart parts)

-- | Group ascending, consecutive columns into maximal @(start, length)@
-- runs.
runs :: [Int] -> [(Int, Int)]
runs = List.foldl' step []
  where
    step [] n = [(n, 1)]
    step acc@((s, l) : rest) n
      | s + l == n = (s, l + 1) : rest
      | otherwise = (n, 1) : acc

----------------------------------------------------------------------------
-- Composition
----------------------------------------------------------------------------

-- | Place a source canvas onto a destination at offset @(dx, dy)@.
-- The result has the destination's dimensions. Transparent source
-- positions do not change the destination. Opaque source glyphs
-- overwrite the destination. They clip destination glyphs that they
-- partially cover, with Vty's ellipsis policy. Source glyphs
-- partially outside the destination are clipped the same way. Glyphs
-- wholly outside disappear.
placeCanvas :: Int -> Int -> Canvas -> Canvas -> Canvas
placeCanvas dx dy src dst = Canvas dw dh (Map.unions (map row [0 .. dh - 1]))
  where
    dw = canvasWidth dst
    dh = canvasHeight dst
    srcRows = rowsOf src
    dstRows = rowsOf dst
    row y =
      let srcRow = Map.findWithDefault Map.empty (y - dy) srcRows
          dstRow = Map.findWithDefault Map.empty y dstRows
          srcClipped =
            Map.fromList
              [ (x', c')
              | (x, c) <- Map.toList srcRow
              , (x', c') <- maybeToList (clipInterval 0 dw (x + dx) c)
              ]
          dstClipped =
            Map.fromList
              [ (x', c')
              | (x, c) <- Map.toList dstRow
              , (x', c') <- clipAgainst (coveredColumns srcClipped) x c
              ]
      in Map.mapKeysMonotonic (\x -> (x, y)) (Map.union dstClipped srcClipped)

-- | Shift a canvas by @(dx, dy)@. Dimensions are preserved. Glyphs
-- that move outside bounds are clipped or lost. Vacated positions
-- become transparent.
translate :: Int -> Int -> Canvas -> Canvas
translate dx dy src =
  placeCanvas dx dy src (blankCanvas (canvasWidth src) (canvasHeight src))

-- | Stack canvases in z-order (last = topmost). The result has the
-- first canvas's dimensions; later canvases overlay earlier ones.
stack :: [Canvas] -> Canvas
stack [] = blankCanvas 0 0
stack (c : cs) = List.foldl' (\dst src -> placeCanvas 0 0 src dst) c cs

-- | Group a canvas's cells by row, keyed by glyph start column.
rowsOf :: Canvas -> Map Int (Map Int CanvasCell)
rowsOf = Map.foldlWithKey' insert Map.empty . canvasCells
  where
    insert acc (x, y) cell = Map.insertWith Map.union y (Map.singleton x cell) acc

----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

-- | Terminal default attributes.
defaultFixedAttr :: V.FixedAttr
defaultFixedAttr = V.FixedAttr V.defaultStyleMask Nothing Nothing Nothing

-- | Convert a resolved 'V.FixedAttr' back to a fully specified
-- 'V.Attr' with no 'V.KeepCurrent' fields.
fixedToAttr :: V.FixedAttr -> V.Attr
fixedToAttr f =
  V.Attr
    { V.attrStyle =
        if V.fixedStyle f == V.defaultStyleMask then V.Default else V.SetTo (V.fixedStyle f)
    , V.attrForeColor = maybe V.Default V.SetTo (V.fixedForeColor f)
    , V.attrBackColor = maybe V.Default V.SetTo (V.fixedBackColor f)
    , V.attrURL = maybe V.Default V.SetTo (V.fixedURL f)
    }

-- | Convert a vty 'Image' to an opaque 'Canvas'. This decodes the
-- spans that Vty rendered for the image. Every column becomes a
-- concrete glyph cell, including row-end and background blanks. A
-- zero-area image has no cells.
imageToCanvas :: V.Image -> Canvas
imageToCanvas img
  | w <= 0 || h <= 0 = Canvas w h Map.empty
  | otherwise =
      Canvas
        { canvasWidth = w
        , canvasHeight = h
        , canvasCells =
            Map.fromList
              [ ((x, y), c)
              | (y, row) <- zip [0 ..] (map (decodeRow . Vec.toList) (Vec.toList ops))
              , (x, c) <- Map.toList row
              ]
        }
  where
    w = V.imageWidth img
    h = V.imageHeight img
    ops = PTS.displayOpsForPic (V.picForImage img) (w, h)

-- | Decode one row of span operations into glyph cells. Zero-width
-- characters become suffix runs of the preceding glyph. A zero-width
-- prefix of the row, before the first base character, becomes a
-- prefix of the first glyph.
decodeRow :: [SpanOp] -> Map Int CanvasCell
decodeRow = go defaultFixedAttr 0 Nothing [] Map.empty
  where
    go _ _ _ _ acc [] = acc
    go cur col lastStart pending acc (op : rest) = case op of
      TextSpan attr _ _ tl ->
        let eff = fixDisplayAttr cur attr
            attr' = fixedToAttr eff
            (col', lastStart', pending', acc') =
              decodeText attr' (TL.toStrict tl) col lastStart pending acc
        in go eff col' lastStart' pending' acc' rest
      RowEnd n ->
        let blankAttr = fixedToAttr defaultFixedAttr
            (col', lastStart', pending', acc') =
              decodeText blankAttr (T.replicate n " ") col lastStart pending acc
        in go defaultFixedAttr col' lastStart' pending' acc' rest
      Skip n ->
        error $
          "canvas decode error: unexpected Skip ("
            ++ show n
            ++ " columns) in final picture spans"

-- | Decode attributed text into glyph cells. The column cursor, the
-- start of the last emitted glyph (for zero-width attachment), and
-- any pending zero-width prefix are threaded through.
decodeText
  :: V.Attr
  -> T.Text
  -> Int
  -> Maybe Int
  -> [(V.Attr, T.Text)]
  -> Map Int CanvasCell
  -> (Int, Maybe Int, [(V.Attr, T.Text)], Map Int CanvasCell)
decodeText baseAttr txt startCol startGlyph startPending startCells = T.foldl' step (startCol, startGlyph, startPending, startCells) txt
  where
    step (col, lastStart, pending, acc) ch =
      let cw = wcwidth ch
      in if cw <= 0
           then case lastStart of
             Just ls ->
               ( col
               , lastStart
               , pending
               , Map.adjust (appendRun (baseAttr, T.singleton ch)) ls acc
               )
             Nothing ->
               (col, lastStart, pending ++ [(baseAttr, T.singleton ch)], acc)
           else
             let cell = cellFromRuns (pending ++ [(baseAttr, T.singleton ch)])
             in (col + cw, Just col, [], Map.insert col cell acc)

-- | Append a run to a cell, re-coalescing and recomputing its width.
appendRun :: (V.Attr, T.Text) -> CanvasCell -> CanvasCell
appendRun run c =
  cellFromRuns (NE.toList (canvasCellRuns c) ++ [run])

-- | One horizontal piece of an output row: attributed text or a
-- transparent gap.
data RowPart = TextRun (V.Attr, T.Text) | Gap !Int

-- | Convert a 'Canvas' to a vty 'Image'. Transparent gaps become
-- 'V.backgroundFill', so an underlying image shows through. Each
-- glyph consumes its full column width; no continuation space is
-- emitted. Adjacent equal-attribute text runs are coalesced.
-- Nonpositive area renders as 'V.emptyImage'.
canvasToImage :: Canvas -> V.Image
canvasToImage canvas@(Canvas w h _)
  | w <= 0 || h <= 0 = V.emptyImage
  | otherwise = V.vertCat [rowImage y | y <- [0 .. h - 1]]
  where
    rowsMap = rowsOf canvas
    rowImage y =
      V.horizCat . map partImage . mergeParts $
        walkRow 0 (Map.toList (Map.findWithDefault Map.empty y rowsMap))
    walkRow col [] = [Gap (w - col) | w - col > 0]
    walkRow col ((x, c) : rest) =
      [Gap (x - col) | x - col > 0]
        ++ map TextRun (NE.toList (canvasCellRuns c))
        ++ walkRow (x + canvasCellColumns c) rest
    mergeParts = List.foldl' merge []
    merge [] p = [p]
    merge acc p = case (last acc, p) of
      (TextRun (a1, t1), TextRun (a2, t2))
        | a1 == a2 -> init acc ++ [TextRun (a1, t1 <> t2)]
      _ -> acc ++ [p]
    partImage (TextRun (attr, txt)) = V.text' attr txt
    partImage (Gap n) = V.backgroundFill n 1
