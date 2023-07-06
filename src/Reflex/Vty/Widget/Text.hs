{-|
  Description: Text- and character-rendering widgets
-}
module Reflex.Vty.Widget.Text where

import Control.Monad.Fix
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Zipper as TZ
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Scroll
import Reflex.Vty.Widget.Input.Mouse

-- | Fill the background with a particular character.
fill :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => Behavior t Char -> m ()
fill bc = do
  dw <- displayWidth
  dh <- displayHeight
  bt <- theme
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


-- TODO delete this and use new local theming
-- | A widget that displays text with custom time-varying attributes
richText
  :: (Reflex t, Monad m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m)
  => RichTextConfig t
  -> Behavior t Text
  -> m ()
richText cfg t = do
  dw <- displayWidth
  let img = (\w a s -> [wrapText w a s])
        <$> current dw
        <*> _richTextConfig_attributes cfg
        <*> t
  tellImages img
  where
    wrapText maxWidth attrs = V.vertCat
      . concatMap (fmap (V.string attrs . T.unpack) . TZ.wrapWithOffset maxWidth 0)
      . T.split (=='\n')

-- | Renders text, wrapped to the container width
text
  :: (Reflex t, Monad m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m)
  => Behavior t Text
  -> m ()
text t = do
  bt <- theme
  richText (RichTextConfig bt) t

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
  :: forall t m. (Reflex t, MonadHold t m, MonadFix m, HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasTheme t m, PostBuild t m)
  => ScrollableConfig t
  -> Dynamic t Text
  -> m (Scrollable t)
scrollableText cfg t = do
  pb <- getPostBuild
  scrollable cfg $ do
    ((), images) <- runImageWriter $ text (current t)
    pure $ (V.vertCat <$> images, () <$ updated t)
