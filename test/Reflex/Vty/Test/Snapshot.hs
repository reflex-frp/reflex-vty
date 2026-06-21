{-# LANGUAGE RecordWildCards #-}

{- |
Description: Rendering snapshot harness for testing vty Image output

Converts a 'V.Image' into a 2D grid of cells, each carrying the display
character and its 'V.Attr'. This lets tests assert on rendered output
without a real terminal — useful for golden-file comparison and precise
cell-level checks.
-}
module Reflex.Vty.Test.Snapshot
  ( Cell (..)
  , Grid
  , imageToGrid
  , imageText
  , renderGrid
  , gridEquals
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import qualified Graphics.Vty as V
import Graphics.Vty.Image.Internal (Image (..))
import Graphics.Text.Width (wcwidth)

-- | A single rendered cell: the display character and its vty attribute.
data Cell = Cell
  { cellChar :: !Char
  , cellAttr :: !V.Attr
  }
  deriving (Eq, Show)

-- | A grid is a list of rows (top-to-bottom), each a list of cells (left-to-right).
type Grid = [[Cell]]

-- | Render a vty 'Image' to a 'Grid'. The grid dimensions match
-- 'V.imageWidth' × 'V.imageHeight'. Any cell not explicitly written by
-- the image (e.g. gaps in a sparse layout) defaults to a space with
-- 'V.defAttr'.
imageToGrid :: Image -> Grid
imageToGrid img =
  let w = V.imageWidth img
      h = V.imageHeight img
      cells = walkImage img 0 0 Map.empty
   in [ [ Map.findWithDefault blankCell (x, y) cells
        | x <- [0 .. max 0 (w - 1)]
        ]
      | y <- [0 .. max 0 (h - 1)]
      ]
  where
    blankCell = Cell ' ' V.defAttr

-- | Extract just the visible text from an image, one string per row.
-- Useful for quick visual inspection in test failures.
imageText :: Image -> [String]
imageText img = map (map cellChar) (imageToGrid img)

-- | Pretty-print a grid for debugging or golden-file comparison.
-- Shows the text grid first, then per-cell attributes (only for cells
-- whose attr differs from 'V.defAttr').
renderGrid :: Grid -> String
renderGrid grid =
  unlines ("--- text ---" : map (map cellChar) grid)
    ++ "--- attrs ---\n"
    ++ attrLines
  where
    attrLines =
      unlines
        [ show (row, col) ++ " " ++ show c ++ " " ++ showAttr (cellAttr c)
        | (row, line) <- zip [0 ..] grid
        , (col, c) <- zip [0 ..] line
        , cellAttr c /= V.defAttr
        ]

-- | Check whether two grids are equal. Returns 'Nothing' if they match,
-- or a description of the first differing cell.
gridEquals :: Grid -> Grid -> Maybe String
gridEquals a b
  | length a /= length b = Just $ "row count: " ++ show (length a) ++ " vs " ++ show (length b)
  | otherwise = go 0 a b
  where
    go _ [] [] = Nothing
    go row (ra : ras) (rb : rbs)
      | length ra /= length rb =
          Just $ "row " ++ show row ++ " width: " ++ show (length ra) ++ " vs " ++ show (length rb)
      | otherwise =
          case findCellDiff row 0 ra rb of
            Just d -> Just d
            Nothing -> go (row + 1) ras rbs
    findCellDiff _ _ [] [] = Nothing
    findCellDiff row col (ca : cas) (cb : cbs)
      | ca /= cb =
          Just $
            "cell ("
              ++ show (row, col)
              ++ "): '"
              ++ [cellChar ca]
              ++ "' "
              ++ showAttr (cellAttr ca)
              ++ " vs '"
              ++ [cellChar cb]
              ++ "' "
              ++ showAttr (cellAttr cb)
      | otherwise = findCellDiff row (col + 1) cas cbs

----------------------------------------------------------------------------
-- Internal: tree walker
----------------------------------------------------------------------------

type CellMap = Map (Int, Int) Cell

walkImage :: Image -> Int -> Int -> CellMap -> CellMap
walkImage img x y acc =
  case img of
    HorizText{..} ->
      let chars = TL.unpack displayText
       in placeText attr chars x y acc
    HorizJoin{..} ->
      let acc' = walkImage partLeft x y acc
       in walkImage partRight (x + V.imageWidth partLeft) y acc'
    VertJoin{..} ->
      let acc' = walkImage partTop x y acc
       in walkImage partBottom x (y + V.imageHeight partTop) acc'
    BGFill{..} ->
      foldl'
        (\m (dx, dy) -> Map.insertWith (\_ old -> old) (x + dx, y + dy) blank m)
        acc
        [ (dx, dy)
        | dx <- [0 .. outputWidth - 1]
        , dy <- [0 .. outputHeight - 1]
        ]
      where
        blank = Cell ' ' V.defAttr
    Crop{..} ->
      let innerCells = walkImage croppedImage 0 0 Map.empty
          visible =
            [ ( (x + kx - leftSkip, y + ky - topSkip)
              , cell
              )
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
            else
              go cs (dx + w) $
                Map.insert (x + dx, y) (Cell c attr) acc

----------------------------------------------------------------------------
-- Internal: attr serialization
----------------------------------------------------------------------------

showAttr :: V.Attr -> String
showAttr a =
  concat
    [ "fg="
    , showMD V.attrForeColor
    , " bg="
    , showMD V.attrBackColor
    , " style="
    , showStyle
    , " url="
    , showMD V.attrURL
    ]
  where
    showMD f = case f a of
      V.Default -> "default"
      V.KeepCurrent -> "keep"
      V.SetTo v -> show v
    styleMask = V.styleMask a
    showStyle =
      let styles =
            [ ("bold", V.bold)
            , ("underline", V.underline)
            , ("reverse", V.reverseVideo)
            , ("blink", V.blink)
            , ("dim", V.dim)
            , ("italic", V.italic)
            , ("strikethrough", V.strikethrough)
            ]
          active = [name | (name, s) <- styles, V.hasStyle styleMask s]
       in if null active then "none" else unwords active
