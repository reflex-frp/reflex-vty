{- |
Module: Reflex.Vty.Theme
Description: Structured theme record for reflex-vty widgets

A 'Theme' is a record of 'Style's for each part of the UI. 'HasTheme' delivers
the ambient 'Theme' to widgets; 'localTheme' overrides it for a subtree.
-}
module Reflex.Vty.Theme (
  Theme (..),
  defTheme,
  darkTheme,
  charmTheme,
  draculaTheme,
  nordTheme,
  zenburnTheme,
  gruvboxTheme,
  themeToAttr,
) where

import Data.Default (Default (..))
import qualified Graphics.Vty as V
import Reflex (Behavior, Reflex)
import Reflex.Vty.Style

-- | A structured theme: one 'Style' per UI element.
data Theme = Theme
  { _theme_default :: Style
  , _theme_border :: Style
  , _theme_title :: Style
  , _theme_link :: Style
  , _theme_buttonFocused :: Style
  , _theme_buttonUnfocused :: Style
  , _theme_checkbox :: Style
  , _theme_textInput :: Style
  , _theme_textInputCursor :: Style
  }

instance Default Theme where
  def = defTheme

-- | The default theme: no colors, no transforms, links underlined.
defTheme :: Theme
defTheme =
  Theme
    { _theme_default = def
    , _theme_border = def
    , _theme_title = withBold def
    , _theme_link = withUnderline UnderlineSingle def
    , _theme_buttonFocused = def
    , _theme_buttonUnfocused = def
    , _theme_checkbox = def
    , _theme_textInput = def
    , _theme_textInputCursor = withReverse def
    }

-- | A dark-background theme matching the legacy @darkTheme :: V.Attr@.
darkTheme :: Theme
darkTheme =
  defTheme
    { _theme_default = withForeground green . withBackground black $ def
    }

-- | The Charm theme: magenta/purple accents on a dark background.
charmTheme :: Theme
charmTheme =
  defTheme
    { _theme_default = withForeground white . withBackground (rgbColor 22 18 30) $ def
    , _theme_border = withForeground magenta def
    , _theme_title = withForeground magenta . withBold $ def
    , _theme_link = withForeground magenta . withUnderline UnderlineSingle $ def
    , _theme_buttonFocused = withForeground magenta . withBold $ def
    , _theme_buttonUnfocused = withForeground white def
    , _theme_textInputCursor = withReverse def
    }

-- | The Dracula theme: pink/cyan/green on a dark background.
draculaTheme :: Theme
draculaTheme =
  defTheme
    { _theme_default = withForeground white . withBackground (rgbColor 40 42 54) $ def
    , _theme_border = withForeground (rgbColor 98 114 164) def
    , _theme_title = withForeground (rgbColor 255 184 108) . withBold $ def
    , _theme_link = withForeground (rgbColor 139 233 253) . withUnderline UnderlineSingle $ def
    , _theme_buttonFocused = withForeground (rgbColor 189 147 249) . withBold $ def
    , _theme_buttonUnfocused = withForeground white def
    , _theme_textInputCursor = withReverse def
    }

-- | The Nord theme: blues and cool greys on a dark slate background.
nordTheme :: Theme
nordTheme =
  defTheme
    { _theme_default = withForeground (rgbColor 216 222 233) . withBackground (rgbColor 46 52 64) $ def
    , _theme_border = withForeground (rgbColor 76 86 106) def
    , _theme_title = withForeground (rgbColor 136 192 208) . withBold $ def
    , _theme_link = withForeground (rgbColor 143 188 187) . withUnderline UnderlineSingle $ def
    , _theme_buttonFocused = withForeground (rgbColor 136 192 208) . withBold $ def
    , _theme_buttonUnfocused = withForeground (rgbColor 216 222 233) def
    , _theme_textInputCursor = withReverse def
    }

-- | The Zenburn theme: muted greens and warm browns on a dark background.
zenburnTheme :: Theme
zenburnTheme =
  defTheme
    { _theme_default = withForeground (rgbColor 220 220 204) . withBackground (rgbColor 64 64 64) $ def
    , _theme_border = withForeground (rgbColor 102 153 153) def
    , _theme_title = withForeground (rgbColor 233 233 191) . withBold $ def
    , _theme_link = withForeground (rgbColor 159 159 95) . withUnderline UnderlineSingle $ def
    , _theme_buttonFocused = withForeground (rgbColor 159 159 95) . withBold $ def
    , _theme_buttonUnfocused = withForeground (rgbColor 220 220 204) def
    , _theme_textInputCursor = withReverse def
    }

-- | The Gruvbox theme: warm earthy tones on a dark background.
gruvboxTheme :: Theme
gruvboxTheme =
  defTheme
    { _theme_default = withForeground (rgbColor 235 219 178) . withBackground (rgbColor 40 40 40) $ def
    , _theme_border = withForeground (rgbColor 152 151 26) def
    , _theme_title = withForeground (rgbColor 250 189 47) . withBold $ def
    , _theme_link = withForeground (rgbColor 131 165 152) . withUnderline UnderlineSingle $ def
    , _theme_buttonFocused = withForeground (rgbColor 214 93 14) . withBold $ def
    , _theme_buttonUnfocused = withForeground (rgbColor 235 219 178) def
    , _theme_textInputCursor = withReverse def
    }

{- | Convenience: extract the ambient 'V.Attr' from the theme's
'_theme_default' 'Style'. Most widgets only need this.
-}
themeToAttr :: (Reflex t) => Behavior t Theme -> Behavior t V.Attr
themeToAttr = fmap (flip applyAttr V.defAttr . _theme_default)
