{-|
Module: Reflex.Vty.Style
Description: Lip Gloss-inspired declarative styling for vty images

A 'Style' is a record of presentation rules (foreground/background color, text
transforms and variants, padding, margin, borders, dimensions, and alignment)
that can be rendered to 'Graphics.Vty.Image'. Styles support inheritance:
'inherit' fills only the fields that are unset in the child, and the 'with*'
setters replace fields.

Use 'render' or 'renderB' to produce an 'Image' from a 'Style' and some text.
-}
module Reflex.Vty.Style
  ( -- * Types
    Style(..)
  , def
    -- ** Sub-types
  , Color
  , UnderlineStyle(..)
  , BorderStyle(..)
  , HAlign(..)
  , VAlign(..)
  , Padding(..)
  , Margin(..)
    -- * Color constants
  , black, red, green, yellow, blue, magenta, cyan, white
  , brightBlack, brightRed, brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan, brightWhite
  , rgbColor
    -- * Border style presets
  , singleBorder, roundedBorder, thickBorder, doubleBorder, asciiBorder, markdownBorder, noBorder
    -- * Setters
    -- ** Color
  , withForeground, withBackground
    -- ** Text transforms
  , withBold, withItalic, withFaint, withBlink, withStrikethrough, withReverse
    -- ** Underline
  , withUnderline, withUnderlineColor
    -- ** Hyperlink
  , withHyperlink
    -- ** Padding and margin
  , withPadding, withPaddingTop, withPaddingRight, withPaddingBottom, withPaddingLeft
  , withMargin, withMarginTop, withMarginRight, withMarginBottom, withMarginLeft
    -- ** Border
  , withBorder, withBorderForeground, withBorderBackground
  , withBorderTop, withBorderBottom, withBorderLeft, withBorderRight
    -- ** Dimensions
  , withWidth, withHeight, withMaxWidth, withMaxHeight
    -- ** Alignment
  , withAlignH, withAlignV
    -- ** Whitespace
  , withWhitespace
    -- * Composition
  , inherit
    -- * Rendering
  , render
  , renderB
  , applyAttr
  , measure
  ) where

import Data.Default (Default(..))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as V.Color
import qualified Graphics.Vty.Image as V.Image
import Reflex (Behavior, Reflex)

-- | A terminal color. Currently an alias for vty's 'V.Color'; this keeps the
-- public API stable if vty's representation changes later.
type Color = V.Color.Color

-- | Underline decoration variants. vty only supports a single
-- 'V.underline' style bit, so 'UnderlineSingle' is the only one rendered
-- distinctly; the others are accepted for API compatibility with Lip Gloss
-- and degrade to single underline (or none) at render time.
data UnderlineStyle
  = UnderlineNone
  | UnderlineSingle
  | UnderlineDouble
  | UnderlineCurly
  | UnderlineDotted
  | UnderlineDashed
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Horizontal alignment within the available width.
data HAlign = HAlignLeft | HAlignCenter | HAlignRight
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Vertical alignment within the available height.
data VAlign = VAlignTop | VAlignMiddle | VAlignBottom
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Padding on the four sides (top, right, bottom, left), in cells.
data Padding = Padding
  { _padding_top    :: !Int
  , _padding_right  :: !Int
  , _padding_bottom :: !Int
  , _padding_left   :: !Int
  } deriving (Eq, Ord, Show)

instance Default Padding where
  def = Padding 0 0 0 0

-- | Margin on the four sides (top, right, bottom, left), in cells. Margin
-- is drawn outside the border using the same whitespace character as
-- padding.
data Margin = Margin
  { _margin_top    :: !Int
  , _margin_right  :: !Int
  , _margin_bottom :: !Int
  , _margin_left   :: !Int
  } deriving (Eq, Ord, Show)

instance Default Margin where
  def = Margin 0 0 0 0

-- | The eight characters that make up a box border. 'Nothing' on a side
-- means the side is not drawn; 'Nothing' on a corner means the corner is
-- not drawn when both adjacent sides are present.
data BorderStyle = BorderStyle
  { _border_top         :: !(Maybe Char)
  , _border_bottom      :: !(Maybe Char)
  , _border_left        :: !(Maybe Char)
  , _border_right       :: !(Maybe Char)
  , _border_topLeft     :: !(Maybe Char)
  , _border_topRight    :: !(Maybe Char)
  , _border_bottomLeft  :: !(Maybe Char)
  , _border_bottomRight :: !(Maybe Char)
  } deriving (Eq, Ord, Show)

-- | A declarative presentational style, modelled on Lip Gloss's @Style@.
-- Every field is 'Maybe' so that 'inherit' can fill only the gaps; use the
-- 'with*' setters to populate fields and 'def' for an empty style.
data Style = Style
  { _style_foreground       :: !(Maybe Color)
  , _style_background       :: !(Maybe Color)
  , _style_bold             :: !(Maybe Bool)
  , _style_italic           :: !(Maybe Bool)
  , _style_faint            :: !(Maybe Bool)
  , _style_blink            :: !(Maybe Bool)
  , _style_strikethrough    :: !(Maybe Bool)
  , _style_reverse          :: !(Maybe Bool)
  , _style_underline        :: !(Maybe UnderlineStyle)
  , _style_underlineColor   :: !(Maybe Color)
  , _style_hyperlink        :: !(Maybe Text)
  , _style_padding          :: !Padding
  , _style_margin           :: !Margin
  , _style_border           :: !(Maybe BorderStyle)
  , _style_borderTop        :: !(Maybe Bool)
  , _style_borderBottom     :: !(Maybe Bool)
  , _style_borderLeft       :: !(Maybe Bool)
  , _style_borderRight      :: !(Maybe Bool)
  , _style_borderForeground :: !(Maybe Color)
  , _style_borderBackground :: !(Maybe Color)
  , _style_width            :: !(Maybe Int)
  , _style_height           :: !(Maybe Int)
  , _style_maxWidth         :: !(Maybe Int)
  , _style_maxHeight        :: !(Maybe Int)
  , _style_alignHorizontal  :: !(Maybe HAlign)
  , _style_alignVertical    :: !(Maybe VAlign)
  , _style_whitespaceChar   :: !(Maybe Char)
  }

instance Default Style where
  def = Style
    { _style_foreground       = Nothing
    , _style_background       = Nothing
    , _style_bold             = Nothing
    , _style_italic           = Nothing
    , _style_faint            = Nothing
    , _style_blink            = Nothing
    , _style_strikethrough    = Nothing
    , _style_reverse          = Nothing
    , _style_underline        = Nothing
    , _style_underlineColor   = Nothing
    , _style_hyperlink        = Nothing
    , _style_padding          = def
    , _style_margin           = def
    , _style_border           = Nothing
    , _style_borderTop        = Nothing
    , _style_borderBottom     = Nothing
    , _style_borderLeft       = Nothing
    , _style_borderRight      = Nothing
    , _style_borderForeground = Nothing
    , _style_borderBackground = Nothing
    , _style_width            = Nothing
    , _style_height           = Nothing
    , _style_maxWidth         = Nothing
    , _style_maxHeight        = Nothing
    , _style_alignHorizontal  = Nothing
    , _style_alignVertical    = Nothing
    , _style_whitespaceChar   = Nothing
    }

-- | Fill the gaps in @child@ with values from @parent@. Only 'Nothing'
-- fields (and zero padding/margin) are inherited; non-'Nothing' fields in
-- @child@ are kept. This mirrors Lip Gloss's @Inherit@ semantics.
inherit :: Style -> Style -> Style
inherit parent child = Style
  { _style_foreground       = pick _style_foreground
  , _style_background       = pick _style_background
  , _style_bold             = pick _style_bold
  , _style_italic           = pick _style_italic
  , _style_faint            = pick _style_faint
  , _style_blink            = pick _style_blink
  , _style_strikethrough    = pick _style_strikethrough
  , _style_reverse          = pick _style_reverse
  , _style_underline        = pick _style_underline
  , _style_underlineColor   = pick _style_underlineColor
  , _style_hyperlink        = pick _style_hyperlink
  , _style_padding          = inheritPadding (_style_padding parent) (_style_padding child)
  , _style_margin           = inheritMargin  (_style_margin  parent) (_style_margin  child)
  , _style_border           = pick _style_border
  , _style_borderTop        = pick _style_borderTop
  , _style_borderBottom     = pick _style_borderBottom
  , _style_borderLeft       = pick _style_borderLeft
  , _style_borderRight      = pick _style_borderRight
  , _style_borderForeground = pick _style_borderForeground
  , _style_borderBackground = pick _style_borderBackground
  , _style_width            = pick _style_width
  , _style_height           = pick _style_height
  , _style_maxWidth         = pick _style_maxWidth
  , _style_maxHeight        = pick _style_maxHeight
  , _style_alignHorizontal  = pick _style_alignHorizontal
  , _style_alignVertical    = pick _style_alignVertical
  , _style_whitespaceChar   = pick _style_whitespaceChar
  }
  where
    pick :: forall a. (Style -> Maybe a) -> Maybe a
    pick f = case f child of
      Just x  -> Just x
      Nothing -> f parent

inheritPadding :: Padding -> Padding -> Padding
inheritPadding parent child = Padding
  { _padding_top    = nz (_padding_top child)    (_padding_top parent)
  , _padding_right  = nz (_padding_right child)  (_padding_right parent)
  , _padding_bottom = nz (_padding_bottom child) (_padding_bottom parent)
  , _padding_left   = nz (_padding_left child)   (_padding_left parent)
  }
  where nz c p = if c /= 0 then c else p

inheritMargin :: Margin -> Margin -> Margin
inheritMargin parent child = Margin
  { _margin_top    = nz (_margin_top child)    (_margin_top parent)
  , _margin_right  = nz (_margin_right child)  (_margin_right parent)
  , _margin_bottom = nz (_margin_bottom child) (_margin_bottom parent)
  , _margin_left   = nz (_margin_left child)   (_margin_left parent)
  }
  where nz c p = if c /= 0 then c else p

----------------------------------------------------------------------------
-- Color constants (re-exported from vty for convenience)
----------------------------------------------------------------------------

black, red, green, yellow, blue, magenta, cyan, white :: Color
black   = V.Color.black
red     = V.Color.red
green   = V.Color.green
yellow  = V.Color.yellow
blue    = V.Color.blue
magenta = V.Color.magenta
cyan    = V.Color.cyan
white   = V.Color.white

brightBlack, brightRed, brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan, brightWhite :: Color
brightBlack   = V.Color.brightBlack
brightRed     = V.Color.brightRed
brightGreen   = V.Color.brightGreen
brightYellow  = V.Color.brightYellow
brightBlue    = V.Color.brightBlue
brightMagenta = V.Color.brightMagenta
brightCyan    = V.Color.brightCyan
brightWhite   = V.Color.brightWhite

-- | Construct a true-color 'Color' from RGB components.
rgbColor :: Int -> Int -> Int -> Color
rgbColor = V.Color.rgbColor

----------------------------------------------------------------------------
-- Border style presets
----------------------------------------------------------------------------

-- | Single-line box-drawing border: @┌─┐│┘─└│@.
singleBorder :: BorderStyle
singleBorder = BorderStyle (Just '─') (Just '─') (Just '│') (Just '│')
                         (Just '┌') (Just '┐') (Just '└') (Just '┘')

-- | Rounded-corner single-line border: @╭─╮│╯─╰│@.
roundedBorder :: BorderStyle
roundedBorder = BorderStyle (Just '─') (Just '─') (Just '│') (Just '│')
                         (Just '╭') (Just '╮') (Just '╰') (Just '╯')

-- | Thick single-line border: @┏━┓┃┛━┗┃@.
thickBorder :: BorderStyle
thickBorder = BorderStyle (Just '━') (Just '━') (Just '┃') (Just '┃')
                         (Just '┏') (Just '┓') (Just '┗') (Just '┛')

-- | Double-line border: @╔═╗║╝═╚║@.
doubleBorder :: BorderStyle
doubleBorder = BorderStyle (Just '═') (Just '═') (Just '║') (Just '║')
                         (Just '╔') (Just '╗') (Just '╚') (Just '╝')

-- | ASCII-only border using @-|+@.
asciiBorder :: BorderStyle
asciiBorder = BorderStyle (Just '-') (Just '-') (Just '|') (Just '|')
                         (Just '+') (Just '+') (Just '+') (Just '+')

-- | Markdown-table border using @-|@ with no corners.
markdownBorder :: BorderStyle
markdownBorder = BorderStyle (Just '-') (Just '-') (Just '|') (Just '|')
                         Nothing Nothing Nothing Nothing

-- | A border with all sides absent. Useful as a base to enable only
-- specific sides via 'withBorderTop' etc.
noBorder :: BorderStyle
noBorder = BorderStyle Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

----------------------------------------------------------------------------
-- Setters
----------------------------------------------------------------------------

withForeground :: Color -> Style -> Style
withForeground c s = s { _style_foreground = Just c }

withBackground :: Color -> Style -> Style
withBackground c s = s { _style_background = Just c }

withBold :: Style -> Style
withBold s = s { _style_bold = Just True }

withItalic :: Style -> Style
withItalic s = s { _style_italic = Just True }

withFaint :: Style -> Style
withFaint s = s { _style_faint = Just True }

withBlink :: Style -> Style
withBlink s = s { _style_blink = Just True }

withStrikethrough :: Style -> Style
withStrikethrough s = s { _style_strikethrough = Just True }

withReverse :: Style -> Style
withReverse s = s { _style_reverse = Just True }

withUnderline :: UnderlineStyle -> Style -> Style
withUnderline u s = s { _style_underline = Just u }

withUnderlineColor :: Color -> Style -> Style
withUnderlineColor c s = s { _style_underlineColor = Just c }

withHyperlink :: Text -> Style -> Style
withHyperlink url s = s { _style_hyperlink = Just url }

withPadding :: Int -> Int -> Int -> Int -> Style -> Style
withPadding t r b l s = s { _style_padding = Padding t r b l }

withPaddingTop :: Int -> Style -> Style
withPaddingTop n s = s { _style_padding = (_style_padding s) { _padding_top = n } }

withPaddingRight :: Int -> Style -> Style
withPaddingRight n s = s { _style_padding = (_style_padding s) { _padding_right = n } }

withPaddingBottom :: Int -> Style -> Style
withPaddingBottom n s = s { _style_padding = (_style_padding s) { _padding_bottom = n } }

withPaddingLeft :: Int -> Style -> Style
withPaddingLeft n s = s { _style_padding = (_style_padding s) { _padding_left = n } }

withMargin :: Int -> Int -> Int -> Int -> Style -> Style
withMargin t r b l s = s { _style_margin = Margin t r b l }

withMarginTop :: Int -> Style -> Style
withMarginTop n s = s { _style_margin = (_style_margin s) { _margin_top = n } }

withMarginRight :: Int -> Style -> Style
withMarginRight n s = s { _style_margin = (_style_margin s) { _margin_right = n } }

withMarginBottom :: Int -> Style -> Style
withMarginBottom n s = s { _style_margin = (_style_margin s) { _margin_bottom = n } }

withMarginLeft :: Int -> Style -> Style
withMarginLeft n s = s { _style_margin = (_style_margin s) { _margin_left = n } }

withBorder :: BorderStyle -> Style -> Style
withBorder b s = s { _style_border = Just b }

withBorderForeground :: Color -> Style -> Style
withBorderForeground c s = s { _style_borderForeground = Just c }

withBorderBackground :: Color -> Style -> Style
withBorderBackground c s = s { _style_borderBackground = Just c }

withBorderTop :: Bool -> Style -> Style
withBorderTop b s = s { _style_borderTop = Just b }

withBorderBottom :: Bool -> Style -> Style
withBorderBottom b s = s { _style_borderBottom = Just b }

withBorderLeft :: Bool -> Style -> Style
withBorderLeft b s = s { _style_borderLeft = Just b }

withBorderRight :: Bool -> Style -> Style
withBorderRight b s = s { _style_borderRight = Just b }

withWidth :: Int -> Style -> Style
withWidth n s = s { _style_width = Just n }

withHeight :: Int -> Style -> Style
withHeight n s = s { _style_height = Just n }

withMaxWidth :: Int -> Style -> Style
withMaxWidth n s = s { _style_maxWidth = Just n }

withMaxHeight :: Int -> Style -> Style
withMaxHeight n s = s { _style_maxHeight = Just n }

withAlignH :: HAlign -> Style -> Style
withAlignH a s = s { _style_alignHorizontal = Just a }

withAlignV :: VAlign -> Style -> Style
withAlignV a s = s { _style_alignVertical = Just a }

withWhitespace :: Char -> Style -> Style
withWhitespace c s = s { _style_whitespaceChar = Just c }

----------------------------------------------------------------------------
-- Rendering
----------------------------------------------------------------------------

-- | Merge the color and text-transform fields of a 'Style' onto an
-- existing 'V.Attr'. Fields that are 'Nothing' in the 'Style' are left
-- unchanged (the existing 'V.Attr' value is kept). Padding, margin,
-- border, dimensions, and alignment are not handled here — those are
-- layout concerns handled by 'render'. This is used by widgets that want
-- to layer a 'Style' on top of the ambient 'HasTheme' attribute.
applyAttr :: Style -> V.Attr -> V.Attr
applyAttr s attr0 =
  applyUnderline
  $ applyHyperlink
  $ applyReverse
  $ applyStrikethrough
  $ applyBlink
  $ applyFaint
  $ applyItalic
  $ applyBold
  $ applyBackground
  $ applyForeground attr0
  where
    applyForeground a = maybe a (V.withForeColor a) (_style_foreground s)
    applyBackground a = maybe a (V.withBackColor a) (_style_background s)
    applyFlag field st a = case field s of
      Just True -> V.withStyle a st
      _         -> a
    applyBold          = applyFlag _style_bold          V.bold
    applyItalic        = applyFlag _style_italic        V.italic
    applyFaint         = applyFlag _style_faint         V.dim
    applyBlink         = applyFlag _style_blink         V.blink
    applyStrikethrough = applyFlag _style_strikethrough V.strikethrough
    applyReverse       = applyFlag _style_reverse       V.reverseVideo
    applyUnderline a = case _style_underline s of
      Nothing          -> a
      Just UnderlineNone -> a
      Just _             -> V.withStyle a V.underline
    applyHyperlink a = maybe a (V.withURL a) (_style_hyperlink s)

-- | Render some 'Text' with a 'Style' to a vty 'V.Image'. Applies text
-- transforms and colors (via 'applyAttr'), pads and aligns the content,
-- draws the border, applies margin, and finally enforces width/height
-- minimums and max-width/max-height clipping.
render :: Style -> Text -> V.Image
render s content =
  applyMaxSize $ applySize $ applyMargin $ applyBorder $ applyPadding $ applyAlign $ contentImage
  where
    ws = fromMaybe ' ' (_style_whitespaceChar s)
    baseAttr = applyAttr s V.defAttr
    borderAttr = applyAttr (borderStyleAttr s) V.defAttr
    contentImage = V.Image.text' baseAttr content
    -- Whitespace fill of a given width/height using the whitespace char.
    fillImage :: V.Attr -> Int -> Int -> V.Image
    fillImage a w h
      | w <= 0 || h <= 0 = V.Image.emptyImage
      | otherwise        = V.Image.charFill a ws w h
    -- Horizontal alignment of an image within a width.
    placeH :: V.Attr -> Int -> V.Image -> V.Image
    placeH a w img
      | imgW >= w = img
      | otherwise = V.Image.horizCat [leftPad, img, rightPad]
      where
        imgW = V.Image.imageWidth img
        slack = w - imgW
        leftPad = case fromMaybe HAlignLeft (_style_alignHorizontal s) of
          HAlignLeft   -> V.Image.emptyImage
          HAlignCenter -> fillImage a (slack `div` 2) 1
          HAlignRight  -> fillImage a slack 1
        rightPad = case fromMaybe HAlignLeft (_style_alignHorizontal s) of
          HAlignLeft   -> fillImage a slack 1
          HAlignCenter -> fillImage a (slack - slack `div` 2) 1
          HAlignRight  -> V.Image.emptyImage
    -- Vertical alignment of an image within a height.
    placeV :: V.Attr -> Int -> V.Image -> V.Image
    placeV a h img
      | imgH >= h = img
      | otherwise = V.Image.vertCat [topPad, img, bottomPad]
      where
        imgH = V.Image.imageHeight img
        slack = h - imgH
        topPad = case fromMaybe VAlignTop (_style_alignVertical s) of
          VAlignTop    -> V.Image.emptyImage
          VAlignMiddle -> fillImage a 1 (slack `div` 2)
          VAlignBottom -> fillImage a 1 slack
        bottomPad = case fromMaybe VAlignTop (_style_alignVertical s) of
          VAlignTop    -> fillImage a 1 slack
          VAlignMiddle -> fillImage a 1 (slack - slack `div` 2)
          VAlignBottom -> V.Image.emptyImage
    -- Width/height minimums: pad the image out to the requested size.
    applySize img =
      let w = fromMaybe (V.Image.imageWidth img) (_style_width s)
          h = fromMaybe (V.Image.imageHeight img) (_style_height s)
          img' = if V.Image.imageWidth img < w
                   then placeH baseAttr w img
                   else img
          img'' = if V.Image.imageHeight img' < h
                    then placeV baseAttr h img'
                    else img'
      in img''
    -- Alignment within the (possibly widened) box.
    applyAlign img = img
    -- Padding.
    applyPadding img =
      let p = _style_padding s
          top    = fillImage baseAttr (innerW img) (_padding_top p)
          bottom = fillImage baseAttr (innerW img) (_padding_bottom p)
          left   = fillImage baseAttr (_padding_left p) (innerH img)
          right  = fillImage baseAttr (_padding_right p) (innerH img)
          innerW = V.Image.imageWidth
          innerH = V.Image.imageHeight
      in V.Image.vertCat [ top
                         , V.Image.horizCat [ left, img, right ]
                         , bottom
                         ]
    -- Border.
    applyBorder img =
      case _style_border s of
        Nothing -> img
        Just b  -> drawBorder borderAttr b
                     (_style_borderTop s)
                     (_style_borderBottom s)
                     (_style_borderLeft s)
                     (_style_borderRight s)
                     img
    -- Margin (drawn outside the border, using the same whitespace char).
    applyMargin img =
      let m = _style_margin s
          top    = fillImage V.defAttr (V.Image.imageWidth img) (_margin_top m)
          bottom = fillImage V.defAttr (V.Image.imageWidth img) (_margin_bottom m)
          left   = fillImage V.defAttr (_margin_left m) (V.Image.imageHeight img)
          right  = fillImage V.defAttr (_margin_right m) (V.Image.imageHeight img)
      in V.Image.vertCat [ top
                         , V.Image.horizCat [ left, img, right ]
                         , bottom
                         ]
    -- Max-width / max-height clipping.
    applyMaxSize img =
      let clipW = maybe img (\w -> V.Image.crop w (V.Image.imageHeight img) img) (_style_maxWidth s)
          clipH = maybe clipW (\h -> V.Image.crop (V.Image.imageWidth clipW) h clipW) (_style_maxHeight s)
      in clipH

-- | A 'Style' that carries only the border color fields, for use as the
-- 'V.Attr' when drawing border characters.
borderStyleAttr :: Style -> Style
borderStyleAttr s = def
  { _style_foreground = _style_borderForeground s
  , _style_background = _style_borderBackground s
  }

-- | Draw a border around an image, respecting per-side toggles. A side is
-- drawn when the 'BorderStyle' has a character for it and the
-- per-side toggle (if set) is 'True'. When a side's toggle is 'Nothing',
-- the side is drawn iff the 'BorderStyle' defines it. Corners are drawn
-- when both adjacent sides are present and the 'BorderStyle' defines the
-- corner.
drawBorder
  :: V.Attr
  -> BorderStyle
  -> Maybe Bool  -- ^ top toggle
  -> Maybe Bool  -- ^ bottom toggle
  -> Maybe Bool  -- ^ left toggle
  -> Maybe Bool  -- ^ right toggle
  -> V.Image
  -> V.Image
drawBorder attr b mTop mBot mLeft mRight img =
  V.Image.vertCat [ topRow, middleRow, bottomRow ]
  where
    w = V.Image.imageWidth img
    topOn    = sideOn _border_top    mTop
    bottomOn = sideOn _border_bottom mBot
    leftOn   = sideOn _border_left   mLeft
    rightOn  = sideOn _border_right  mRight
    sideOn f mt = maybe (isJust (f b)) id mt
    topChar    = _border_top b    >>= \c -> if topOn    then Just (V.Image.char attr c)    else Nothing
    bottomChar = _border_bottom b >>= \c -> if bottomOn then Just (V.Image.char attr c)    else Nothing
    leftChar   = _border_left b   >>= \c -> if leftOn   then Just (V.Image.char attr c)    else Nothing
    rightChar  = _border_right b  >>= \c -> if rightOn  then Just (V.Image.char attr c)    else Nothing
    topLeftChar     = _border_topLeft b     >>= \c -> if topOn    && leftOn  then Just (V.Image.char attr c) else Nothing
    topRightChar    = _border_topRight b    >>= \c -> if topOn    && rightOn then Just (V.Image.char attr c) else Nothing
    bottomLeftChar  = _border_bottomLeft b  >>= \c -> if bottomOn && leftOn  then Just (V.Image.char attr c) else Nothing
    bottomRightChar = _border_bottomRight b >>= \c -> if bottomOn && rightOn then Just (V.Image.char attr c) else Nothing
    -- Top row: corner, horizontal fill, corner
    topRow = V.Image.horizCat $
      maybe [] (:[]) topLeftChar ++
      maybe [] (\c -> [V.Image.horizCat (replicate (max 0 w) c)]) topChar ++
      maybe [] (:[]) topRightChar
    -- Middle row: left char, content, right char
    middleRow = V.Image.horizCat $
      maybe [] (:[]) leftChar ++ [img] ++ maybe [] (:[]) rightChar
    -- Bottom row: corner, horizontal fill, corner
    bottomRow = V.Image.horizCat $
      maybe [] (:[]) bottomLeftChar ++
      maybe [] (\c -> [V.Image.horizCat (replicate (max 0 w) c)]) bottomChar ++
      maybe [] (:[]) bottomRightChar

-- | Reactive variant of 'render' for the common widget case.
renderB :: Reflex t => Behavior t Style -> Behavior t Text -> Behavior t V.Image
renderB bs bt = render <$> bs <*> bt

-- | The width and height (in terminal cells) that 'render' will produce
-- for the given 'Style' and 'Text', /before/ max-width/max-height
-- clipping. Useful for layout-sensitive widgets that need to measure
-- before rendering.
measure :: Style -> Text -> (Int, Int)
measure s content =
  ( totalW, totalH )
  where
    contentW = T.length content  -- approximation; wcwidth would be more accurate
    contentH = 1
    p = _style_padding s
    m = _style_margin s
    borderWidth = if hasBorder then 2 else 0
    borderHeight = if hasBorder then 2 else 0
    hasBorder = maybe False (const True) (_style_border s)
              || any id [ fromMaybe False (_style_borderTop s)
                        , fromMaybe False (_style_borderBottom s)
                        , fromMaybe False (_style_borderLeft s)
                        , fromMaybe False (_style_borderRight s)
                        ]
    minW = fromMaybe 0 (_style_width s)
    minH = fromMaybe 0 (_style_height s)
    innerW = max minW contentW + _padding_left p + _padding_right p
    innerH = max minH contentH + _padding_top p + _padding_bottom p
    totalW = innerW + borderWidth + _margin_left m + _margin_right m
    totalH = innerH + borderHeight + _margin_top m + _margin_bottom m
