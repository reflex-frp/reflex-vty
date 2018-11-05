{-|
Module: Reflex.Vty.Widget.Input
Description: User input widgets for reflex-vty
-}
module Reflex.Vty.Widget.Input
  ( module Export
  , module Reflex.Vty.Widget.Input
  ) where


import Reflex.Vty.Widget.Input.Text as Export

import Data.Default
import Data.Text (Text)
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty.Widget

-- | Configuration options for the 'button' widget
data ButtonConfig t = ButtonConfig
  { _buttonConfig_boxStyle :: Behavior t BoxStyle
  }

instance Reflex t => Default (ButtonConfig t) where
  def = ButtonConfig (pure def)

-- | A button widget that contains a sub-widget
button
  :: (Reflex t, Monad m)
  => ButtonConfig t
  -> VtyWidget t m ()
  -> VtyWidget t m (Event t MouseUp)
button cfg child = do
  box (_buttonConfig_boxStyle cfg) child
  mouseUp

-- | A button widget that displays text that can change
textButton
  :: (Reflex t, Monad m)
  => ButtonConfig t
  -> Behavior t Text
  -> VtyWidget t m (Event t MouseUp)
textButton cfg = button cfg . text -- TODO Centering etc.

-- | A button widget that displays a static bit of text
textButtonStatic
  :: (Reflex t, Monad m)
  => ButtonConfig t
  -> Text
  -> VtyWidget t m (Event t MouseUp)
textButtonStatic cfg = textButton cfg . pure

-- | A clickable link widget
link
  :: (Reflex t, Monad m)
  => Behavior t Text
  -> VtyWidget t m (Event t MouseUp)
link t = do
  let cfg = RichTextConfig
        { _richTextConfig_attributes = pure $ V.withStyle V.defAttr V.underline
        }
  richText cfg t
  mouseUp
