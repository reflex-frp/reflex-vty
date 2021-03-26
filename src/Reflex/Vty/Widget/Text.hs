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
import Reflex.Vty.Widget.Input.Mouse

-- | Fill the background with a particular character.
fill :: (HasDisplayRegion t m, HasImageWriter t m) => Behavior t Char -> m ()
fill bc = do
  dw <- displayWidth
  dh <- displayHeight
  let fillImg =
        (\w h c -> [V.charFill V.defAttr c w h])
        <$> current dw
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
  :: (Reflex t, Monad m, HasDisplayRegion t m, HasImageWriter t m)
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
  :: (Reflex t, Monad m, HasDisplayRegion t m, HasImageWriter t m)
  => Behavior t Text
  -> m ()
text = richText def

-- | Scrollable text widget. The output pair exposes the current scroll position and total number of lines (including those
-- that are hidden)
scrollableText
  :: forall t m. (Reflex t, MonadHold t m, MonadFix m, HasDisplayRegion t m, HasInput t m, HasImageWriter t m)
  => Event t Int
  -- ^ Number of lines to scroll by
  -> Behavior t Text
  -> m (Behavior t (Int, Int))
  -- ^ (Current scroll position, total number of lines)
scrollableText scrollBy t = do
  dw <- displayWidth
  let imgs = wrap <$> current dw <*> t
  kup <- key V.KUp
  kdown <- key V.KDown
  m <- mouseScroll
  let requestedScroll :: Event t Int
      requestedScroll = leftmost
        [ 1 <$ kdown
        , (-1) <$ kup
        , ffor m $ \case
            ScrollDirection_Up -> (-1)
            ScrollDirection_Down -> 1
        , scrollBy
        ]
      updateLine maxN delta ix = min (max 0 (ix + delta)) maxN
  lineIndex :: Dynamic t Int <- foldDyn (\(maxN, delta) ix -> updateLine (maxN - 1) delta ix) 0 $
    attach (length <$> imgs) requestedScroll
  tellImages $ fmap ((:[]) . V.vertCat) $ drop <$> current lineIndex <*> imgs
  return $ (,) <$> ((+) <$> current lineIndex <*> pure 1) <*> (length <$> imgs)
  where
    wrap maxWidth = concatMap (fmap (V.string V.defAttr . T.unpack) . TZ.wrapWithOffset maxWidth 0) . T.split (=='\n')

-- | Renders any behavior whose value can be converted to
-- 'String' as text
display
  :: (Reflex t, Monad m, Show a, HasDisplayRegion t m, HasImageWriter t m)
  => Behavior t a
  -> m ()
display a = text $ T.pack . show <$> a
