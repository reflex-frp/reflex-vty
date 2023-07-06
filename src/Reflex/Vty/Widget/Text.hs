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

data ScrollToBottom
  = ScrollToBottom_Always
  -- ^ Always scroll to the bottom on new output
  | ScrollToBottom_Maintain
  -- ^ Scroll down with new output only when, prior to the new output being
  -- added, the widget was scrolled all the way to the bottom.
  deriving (Eq, Ord, Show)

-- | Configuration for the scrollable text element. Controls scroll behavior.
data ScrollableTextConfig t = ScrollableTextConfig
  { _scrollableTextConfig_scrollBy :: Event t Int
  -- ^ Number of lines to scroll by
  , _scrollableTextConfig_scrollTo :: Event t ScrollPos
  -- ^ Specific position to scroll to
  , _scrollableTextConfig_startingPosition :: ScrollPos
  -- ^ The initial scroll position
  , _scrollableTextConfig_scrollToBottom :: Behavior t (Maybe ScrollToBottom)
  -- ^ How the scroll position should be adjusted as new content is added
  }

instance Reflex t => Default (ScrollableTextConfig t) where
  def = ScrollableTextConfig never never ScrollPos_Top (pure Nothing)

-- | The scroll position
data ScrollPos = ScrollPos_Top | ScrollPos_Line Int | ScrollPos_Bottom
  deriving (Show, Eq, Ord)

-- | The output of a 'scrollableText', indicating its current scroll position.
data ScrollableText t = ScrollableText
  { _scrollableText_scrollPosition :: Behavior t ScrollPos
  , _scrollableText_totalLines :: Behavior t Int
  , _scrollableText_scrollHeight :: Behavior t Int
  }

-- | Scrollable text widget. The output pair exposes the current scroll position and total number of lines (including those
-- that are hidden)
scrollableText
  :: forall t m. (Reflex t, MonadHold t m, MonadFix m, HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasTheme t m)
  => ScrollableTextConfig t
  -> Dynamic t Text
  -> m (ScrollableText t)
scrollableText (ScrollableTextConfig scrollBy scrollTo startingPos onAppend) t = do
  dw <- displayWidth
  bt <- theme
  let imgs = wrap <$> bt <*> current dw <*> current t
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
  dh <- displayHeight
  lineIndex <- foldDynMaybe ($) startingPos $ leftmost
    [ (\((totalLines, h), d) sp -> Just $ scrollByLines sp totalLines h d) <$> attach ((,) <$> (length <$> imgs) <*> current dh) requestedScroll
    , (\((totalLines, h), newScrollPosition) _ -> Just $ case newScrollPosition of
        ScrollPos_Line n -> scrollToLine totalLines h n
        ScrollPos_Top -> ScrollPos_Top
        ScrollPos_Bottom -> ScrollPos_Bottom
      ) <$> attach ((,) <$> (length <$> imgs) <*> current dh) scrollTo
    , (\cfg sp -> case cfg of
        Just ScrollToBottom_Always -> case sp of
          ScrollPos_Bottom -> Nothing
          _ -> Just ScrollPos_Bottom
        _ -> Nothing) <$> tag onAppend (updated t)
    ]
  let imgsToTell height scrollPos images = case scrollPos of
        ScrollPos_Bottom -> drop (max 0 (length images - height)) images
        ScrollPos_Top -> take height images
        ScrollPos_Line n -> drop n images
  tellImages $ fmap ((:[]) . V.vertCat) $ imgsToTell <$> current dh <*> current lineIndex <*> imgs
  return $ ScrollableText
    { _scrollableText_scrollPosition = current lineIndex
    , _scrollableText_totalLines = length <$> imgs
    , _scrollableText_scrollHeight = current dh
    }
  where
    wrap attr maxWidth = concatMap (fmap (V.string attr . T.unpack) . TZ.wrapWithOffset maxWidth 0) . T.split (=='\n')

-- | Modify the scroll position by the given number of lines
scrollByLines :: ScrollPos -> Int -> Int -> Int -> ScrollPos
scrollByLines sp totalLines height delta =
  let newPos = min (max 0 (start sp + delta)) totalLines
  in scrollToLine totalLines height newPos
  where
    start ScrollPos_Top = 0
    start ScrollPos_Bottom = totalLines - height
    start (ScrollPos_Line n) = n

-- | Scroll to a particular line
scrollToLine :: Int -> Int -> Int -> ScrollPos
scrollToLine totalLines height newPos = if
  | totalLines <= height -> ScrollPos_Top
  | newPos == 0 -> ScrollPos_Top
  | newPos + height >= totalLines -> ScrollPos_Bottom
  | otherwise -> ScrollPos_Line newPos

-- | Renders any behavior whose value can be converted to
-- 'String' as text
display
  :: (Reflex t, Monad m, Show a, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m)
  => Behavior t a
  -> m ()
display a = text $ T.pack . show <$> a
