{-|
Module: Reflex.Vty.Theme
Description: Structured theme record for reflex-vty widgets

A 'Theme' is a record of 'Style's for each part of the UI. 'HasTheme' delivers
the ambient 'Theme' to widgets; 'localTheme' overrides it for a subtree.
-}
module Reflex.Vty.Theme
  ( Theme(..)
  , defTheme
  , darkTheme
  , themeToAttr
  ) where

import Data.Default (Default(..))
import qualified Graphics.Vty as V
import Reflex (Behavior, Reflex)
import Reflex.Vty.Style

-- | A structured theme: one 'Style' per UI element.
data Theme = Theme
  { _theme_default         :: Style
  , _theme_border          :: Style
  , _theme_title           :: Style
  , _theme_link            :: Style
  , _theme_buttonFocused   :: Style
  , _theme_buttonUnfocused :: Style
  , _theme_checkbox        :: Style
  , _theme_textInput       :: Style
  , _theme_textInputCursor :: Style
  }

instance Default Theme where
  def = defTheme

-- | The default theme: no colors, no transforms, links underlined.
defTheme :: Theme
defTheme = Theme
  { _theme_default         = def
  , _theme_border          = def
  , _theme_title           = withBold def
  , _theme_link            = withUnderline UnderlineSingle def
  , _theme_buttonFocused   = def
  , _theme_buttonUnfocused = def
  , _theme_checkbox        = def
  , _theme_textInput       = def
  , _theme_textInputCursor = withReverse def
  }

-- | A dark-background theme matching the legacy @darkTheme :: V.Attr@.
darkTheme :: Theme
darkTheme = defTheme
  { _theme_default = withForeground black . withBackground green . withReverse $ def
  }

-- | Convenience: extract the ambient 'V.Attr' from the theme's
-- '_theme_default' 'Style'. Most widgets only need this.
themeToAttr :: Reflex t => Behavior t Theme -> Behavior t V.Attr
themeToAttr = fmap (flip applyAttr V.defAttr . _theme_default)
