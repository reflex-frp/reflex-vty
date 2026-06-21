-- |
--   Description: Text- and character-rendering widgets
module Reflex.Vty.Widget.Text where

import Control.Monad.Fix
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex

import Data.Text.Zipper as TZ
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Scroll

-- | Fill the background with a particular character.
fill :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => Behavior t Char -> m ()
fill bc = do
  dw <- displayWidth
  dh <- displayHeight
  bt <- themeAttr
  let fillImg =
        (\attr w h c -> [V.charFill attr c w h])
          <$> bt
          <*> current dw
          <*> current dh
          <*> bc
  tellImages fillImg

-- | Configuration options for displaying "rich" text
data RichTextConfig t = RichTextConfig
  { _richTextConfig_attributes :: Behavior t V.Attr
  }

instance Reflex t => Default (RichTextConfig t) where
  def = RichTextConfig $ pure V.defAttr

-- | A widget that displays text with custom time-varying attributes
richText
  :: (Reflex t, Monad m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m)
  => RichTextConfig t
  -> Behavior t Text
  -> m ()
richText cfg t = do
  dw <- displayWidth
  let img =
        (\w a s -> [wrapTextImage TextAlignment_Left w a s])
          <$> current dw
          <*> _richTextConfig_attributes cfg
          <*> t
  tellImages img

-- | Renders text, wrapped to the container width
text
  :: (Reflex t, Monad m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m)
  => Behavior t Text
  -> m ()
text = textWithAlignment TextAlignment_Left

-- | Like 'text' but with explicit 'TextAlignment'. The alignment is
-- applied to each logical line after wrapping; see
-- 'Data.Text.Zipper.wrapWithOffsetAndAlignment' for details.
textWithAlignment
  :: (Reflex t, Monad m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m)
  => TextAlignment
  -> Behavior t Text
  -> m ()
textWithAlignment alignment t = do
  dw <- displayWidth
  bt <- themeAttr
  let img =
        (\w a s -> [wrapTextImage alignment w a s])
          <$> current dw
          <*> bt
          <*> t
  tellImages img

-- | Pure helper: wrap a 'Text' to the given width with the given
-- 'TextAlignment' and render it as a single 'V.Image' using the supplied
-- 'V.Attr'. Newlines split logical lines; each logical line is wrapped and
-- aligned independently.
wrapTextImage :: TextAlignment -> Int -> V.Attr -> Text -> V.Image
wrapTextImage alignment maxWidth attrs =
  V.vertCat
    . concatMap (fmap (V.string attrs . T.unpack) . wrapLine)
    . T.split (== '\n')
  where
    wrapLine =
      map _wrappedLines_text
        . wrapWithOffsetAndAlignment alignment maxWidth 0

-- | Renders any behavior whose value can be converted to
-- 'String' as text
display
  :: (Reflex t, Monad m, Show a, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m)
  => Behavior t a
  -> m ()
display a = text $ T.pack . show <$> a

-- | Scrollable text widget. The output exposes the current scroll position and
-- total number of lines (including those that are hidden)
scrollableText
  :: forall t m
   . (Reflex t, MonadHold t m, MonadFix m, HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasTheme t m, PostBuild t m)
  => ScrollableConfig t
  -> Dynamic t Text
  -> m (Scrollable t)
scrollableText cfg t = fmap fst $ scrollable cfg $ do
  text (current t)
  pure (() <$ updated t, ())
