module Reflex.Vty.Style where

data Style = Style
  { _style_foreground :: Maybe Color
  , _style_background :: Maybe Color
  , _style_bold :: Maybe Bool
  , _style_italic :: Maybe Bool
  , _style_faint :: Maybe Bool
  , _style_blink :: Maybe Bool
  , _style_strikethrough :: Maybe Bool
  , _style_reverse :: Maybe Bool
  , _style_underline :: Maybe UnderlineStyle
  , _style_underlineColor :: Maybe Color
  , _style_hyperlink :: Maybe Text -- OSC 8
  , _style_padding :: Padding -- top/right/bottom/left
  , _style_margin :: Margin
  , _style_border :: Maybe BorderStyle
  , _style_borderTop :: Maybe Bool -- per-side toggles
  , _style_borderBottom :: Maybe Bool
  , _style_borderLeft :: Maybe Bool
  , _style_borderRight :: Maybe Bool
  , _style_borderForeground:: Maybe Color
  , _style_borderBackground:: Maybe Color
  , _style_width :: Maybe Int -- minimum
  , _style_height :: Maybe Int
  , _style_maxWidth :: Maybe Int -- hard clip
  , _style_maxHeight :: Maybe Int
  , _style_alignHorizontal :: Maybe HAlign
  , _style_alignVertical :: Maybe VAlign
  , _style_whitespaceChar :: Maybe Char -- fill char for padding/margin/Place
  }

data UnderlineStyle
  = UnderlineNone
  | UnderlineSingle
  | UnderlineDouble
  | UnderlineCurly
  | UnderlineDotted
  | UnderlineDashed

data BorderStyle = BorderStyle
 { _border_top :: Maybe Char
 , _border_bottom :: Maybe Char
 , _border_left :: Maybe Char
 , _border_right :: Maybe Char
 , _border_topLeft :: Maybe Char
 , _border_topRight :: Maybe Char
 , _border_bottomLeft :: Maybe Char
 , _border_bottomRight :: Maybe Char
 }

