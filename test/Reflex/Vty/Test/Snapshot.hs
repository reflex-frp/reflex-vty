{-# LANGUAGE ScopedTypeVariables #-}

-- |
--
-- Renders a 'V.Picture' (or 'V.Image') through Vty's own renderer
-- ("Graphics.Vty.PictureToSpans" 'displayOpsForPic') and decodes the
-- resulting spans into a test-friendly 'Grid'. The renderer produces
-- the spans, so continuation columns and zero-width marks need no
-- reconstruction. A span keeps its complete text and its reported
-- column width.
--
-- Each row is a list of 'RenderSpan's covering the full row width.
-- Trailing blank columns ('RowEnd') become default-attribute blank
-- spans. Effective (fully resolved) attributes are computed per row,
-- starting from terminal defaults, exactly as Vty's output stage does.
--
-- Golden file format (produced by 'renderGrid'):
--
-- > 4x1
-- > "a\30028b"
-- > --- spans ---
-- > 0:0+4 text="a\30028b" attr=FixedAttr {fixedStyle = 0, ...}
--
-- The first line is @WxH@. Then comes one Haskell-escaped row string
-- per row (@show@ of the row's 'String'). Then comes @--- spans ---@
-- and one line per canonical span:
-- @row:column+width text=<quoted> attr=<show FixedAttr>@.
-- Default attributes are included, as well as explicit colors,
-- styles, and URLs.
module Reflex.Vty.Test.Snapshot
  ( RenderSpan (..)
  , Grid (..)
  , pictureToGrid
  , imageToGrid
  , imageText
  , renderGrid
  , gridEquals
  , attrAt
  , GoldenMode (..)
  , assertGolden
  , goldenUpdateCommand
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Graphics.Vty.DisplayAttributes (fixDisplayAttr)
import qualified Graphics.Vty.PictureToSpans as PTS
import Graphics.Vty.Span (SpanOp (RowEnd, Skip, TextSpan))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), (</>))
import Test.Hspec (Expectation, expectationFailure, shouldBe)

-- | A rendered span: its total column width on screen, its complete
-- text (including any zero-width marks), and its fully resolved
-- attribute.
data RenderSpan = RenderSpan
  { renderSpanColumns :: !Int
  , renderSpanText :: !T.Text
  , renderSpanAttr :: !V.FixedAttr
  }
  deriving (Eq, Show)

-- | A rendered grid: requested dimensions plus one row of spans per
-- display row. Every row's spans cover exactly 'gridWidth' columns.
data Grid = Grid
  { gridWidth :: !Int
  , gridHeight :: !Int
  , gridRows :: ![[RenderSpan]]
  }
  deriving (Eq, Show)

-- | Terminal default attributes; the starting point of attribute
-- resolution on each row.
defaultFixedAttr :: V.FixedAttr
defaultFixedAttr = V.FixedAttr V.defaultStyleMask Nothing Nothing Nothing

-- | Render a picture into a 'Grid' of the requested size, using Vty's
-- own renderer. Requested negative dimensions are clamped to zero. A
-- zero-area region yields a grid with those (nonnegative) dimensions
-- and no rows. A picture with no layers is normalized to a single
-- 'V.emptyImage' layer. Vty rejects a nonzero-sized picture with no
-- layers.
pictureToGrid :: V.DisplayRegion -> V.Picture -> Grid
pictureToGrid (reqW, reqH) pic0
  | w == 0 || h == 0 = Grid w h []
  | otherwise = Grid w h padded
  where
    w = max 0 reqW
    h = max 0 reqH
    pic = case pic0 of
      V.Picture {V.picLayers = []} -> pic0 {V.picLayers = [V.emptyImage]}
      _ -> pic0
    rows = map (decodeRow w . Vec.toList) (Vec.toList (PTS.displayOpsForPic pic (w, h)))
    padded = rows ++ replicate (h - length rows) [blankSpan w]

-- | Decode one row of span operations into canonical 'RenderSpan's,
-- resolving attributes from terminal defaults and converting trailing
-- blank columns to default-attribute blanks. The final picture stage
-- must not produce 'Skip' operations. A 'Skip' is an oracle error,
-- not a rendering result.
decodeRow :: Int -> [SpanOp] -> [RenderSpan]
decodeRow width ops =
  padTo width . coalesceSpans . snd $ foldl' step (defaultFixedAttr, []) ops
  where
    step (cur, acc) op = case op of
      TextSpan attr w _ tl ->
        let eff = fixDisplayAttr cur attr
        in (eff, acc ++ [RenderSpan w (TL.toStrict tl) eff])
      RowEnd n -> (defaultFixedAttr, acc ++ [blankSpan n])
      Skip n ->
        error $
          "oracle error: unexpected Skip ("
            ++ show n
            ++ " columns) in final picture spans"

-- | A blank span of @n@ default-attribute columns.
blankSpan :: Int -> RenderSpan
blankSpan n = RenderSpan n (T.replicate n (T.singleton ' ')) defaultFixedAttr

-- | Merge adjacent spans with equal effective attributes without
-- changing their text or widths.
coalesceSpans :: [RenderSpan] -> [RenderSpan]
coalesceSpans = foldl' step []
  where
    step [] s = [s]
    step acc s
      | renderSpanAttr s == renderSpanAttr (last acc) =
          init acc
            ++ [ RenderSpan
                   (renderSpanColumns (last acc) + renderSpanColumns s)
                   (renderSpanText (last acc) <> renderSpanText s)
                   (renderSpanAttr s)
               ]
      | otherwise = acc ++ [s]

-- | Ensure a row's spans cover the full width by appending
-- default-attribute blanks if the renderer left the row short.
padTo :: Int -> [RenderSpan] -> [RenderSpan]
padTo width ss =
  let covered = sum (map renderSpanColumns ss)
  in if covered < width then ss ++ [blankSpan (width - covered)] else ss

-- | Render an image to a grid using the image's own dimensions.
imageToGrid :: V.Image -> Grid
imageToGrid img =
  pictureToGrid (V.imageWidth img, V.imageHeight img) (V.picForImage img)

-- | Extract the visible text of an image, one string per row. The
-- text includes actual trailing blank columns. It never invents a
-- space for a wide-glyph continuation column.
imageText :: V.Image -> [String]
imageText img =
  map (T.unpack . T.concat . map renderSpanText) (gridRows (imageToGrid img))

-- | Look up the effective attribute at a cell coordinate. A wide
-- glyph's continuation column reports the same attribute as its start
-- column. Both columns are covered by the same span. Out-of-bounds
-- coordinates return 'Nothing'.
attrAt :: Int -> Int -> Grid -> Maybe V.FixedAttr
attrAt x y (Grid w h rows)
  | x < 0 || y < 0 || x >= w || y >= h = Nothing
  | otherwise = case drop y rows of
      [] -> Nothing
      (r : _) -> go 0 r
  where
    go _ [] = Nothing
    go col (s : ss)
      | x < col + renderSpanColumns s = Just (renderSpanAttr s)
      | otherwise = go (col + renderSpanColumns s) ss

-- | Check whether two grids are equal. Returns 'Nothing' when they
-- match, or a description of the first difference in dimensions, text,
-- widths, or effective attributes (including URLs).
gridEquals :: Grid -> Grid -> Maybe String
gridEquals a b
  | gridWidth a /= gridWidth b =
      Just $ "width: " ++ show (gridWidth a) ++ " vs " ++ show (gridWidth b)
  | gridHeight a /= gridHeight b =
      Just $ "height: " ++ show (gridHeight a) ++ " vs " ++ show (gridHeight b)
  | otherwise = firstDiff 0 (gridRows a) (gridRows b)
  where
    firstDiff _ [] [] = Nothing
    firstDiff y (ra : ras) (rb : rbs) = case compareRows y ra rb of
      Just d -> Just d
      Nothing -> firstDiff (y + 1) ras rbs
    firstDiff y [] _ = Just ("row " ++ show y ++ ": missing")
    firstDiff y _ [] = Just ("row " ++ show y ++ ": extra")
    compareRows y ra rb
      | rowText ra /= rowText rb =
          Just $
            "row "
              ++ show y
              ++ " text: "
              ++ show (rowText ra)
              ++ " vs "
              ++ show (rowText rb)
      | ra /= rb =
          Just $
            "row "
              ++ show y
              ++ " spans: "
              ++ show ra
              ++ " vs "
              ++ show rb
      | otherwise = Nothing
    rowText = T.unpack . T.concat . map renderSpanText

-- | Pretty-print a grid for golden-file comparison and debugging.
-- See the module documentation for the exact format.
renderGrid :: Grid -> String
renderGrid grid =
  unlines (header : rowStrings)
    ++ unlines ("--- spans ---" : spanLines)
  where
    header = show (gridWidth grid) ++ "x" ++ show (gridHeight grid)
    rowStrings =
      [ show (T.unpack (T.concat (map renderSpanText row)))
      | row <- gridRows grid
      ]
    spanLines =
      [ show y
          ++ ":"
          ++ show col
          ++ "+"
          ++ show (renderSpanColumns s)
          ++ " text="
          ++ show (T.unpack (renderSpanText s))
          ++ " attr="
          ++ show (renderSpanAttr s)
      | (y, row) <- zip [0 :: Int ..] (gridRows grid)
      , (col, s) <- zip (scanl (+) 0 (map renderSpanColumns row)) row
      ]

----------------------------------------------------------------------------
-- Golden file comparison
----------------------------------------------------------------------------

-- | Whether golden fixtures are being checked (the default, and the
-- only mode allowed in CI) or explicitly updated.
data GoldenMode = CheckGoldens | UpdateGoldens
  deriving (Eq, Show)

-- | The exact command that creates or updates golden fixtures.
goldenUpdateCommand :: String
goldenUpdateCommand =
  "REFLEX_VTY_GOLDEN_UPDATE=1 cabal test reflex-vty-test"
    ++ " --test-show-details=direct --test-option=--match=Golden"

-- | Compare the serialized actual output against the golden fixture
-- @\<dir>\</\<name>.txt@. In 'UpdateGoldens' mode, write the output
-- instead of comparing.
--
-- In 'CheckGoldens' mode, create nothing. A missing file fails with
-- the path and 'goldenUpdateCommand'. A mismatch fails through Hspec
-- equality, so the expected and actual contents are visible. IO,
-- permission, and encoding errors fail with the path in the message.
--
-- In 'UpdateGoldens' mode, create the directory. Write changed or
-- missing files in UTF-8. Leave equal files untouched.
assertGolden :: GoldenMode -> FilePath -> String -> String -> Expectation
assertGolden mode dir name actual = do
  let path = dir </> name <.> "txt"
  case mode of
    CheckGoldens -> do
      exists <- doesFileExist path
      if not exists
        then
          expectationFailure $
            "missing golden file "
              ++ path
              ++ "; to create or update it, run: "
              ++ goldenUpdateCommand
        else do
          er <- readGolden path
          case er of
            Left err -> expectationFailure err
            Right expected -> expected `shouldBe` actual
    UpdateGoldens -> do
      createDirectoryIfMissing True dir
      exists <- doesFileExist path
      if not exists
        then do
          merr <- writeGolden path actual
          maybe (pure ()) expectationFailure merr
        else do
          er <- readGolden path
          case er of
            Left err -> expectationFailure err
            Right expected
              | expected /= actual -> do
                  merr <- writeGolden path actual
                  maybe (pure ()) expectationFailure merr
              | otherwise -> pure ()

readGolden :: FilePath -> IO (Either String String)
readGolden path = do
  r <- try (TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile path)
  pure $ case r of
    Left (e :: IOException) ->
      Left ("could not read golden file " ++ path ++ ": " ++ show e)
    Right txt -> Right (T.unpack txt)

writeGolden :: FilePath -> String -> IO (Maybe String)
writeGolden path contents = do
  r <- try (BS.writeFile path (TE.encodeUtf8 (T.pack contents)))
  pure $ case r of
    Left (e :: IOException) ->
      Just ("could not write golden file " ++ path ++ ": " ++ show e)
    Right () -> Nothing
