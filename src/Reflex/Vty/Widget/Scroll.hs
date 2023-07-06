{-| Description: Widgets that scroll when their contents don't fit
-}
module Reflex.Vty.Widget.Scroll where

import Control.Monad.Fix
import Data.Default
import Data.Text (Text)
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Input.Mouse

-- | Configuration options for automatic scroll-to-bottom behavior
data ScrollToBottom
  = ScrollToBottom_Always
  -- ^ Always scroll to the bottom on new output
  | ScrollToBottom_Maintain
  -- ^ Scroll down with new output only when, prior to the new output being
  -- added, the widget was scrolled all the way to the bottom.
  deriving (Eq, Ord, Show)

-- | Configuration for the scrollable element. Controls scroll behavior.
data ScrollableConfig t = ScrollableConfig
  { _scrollableConfig_scrollBy :: Event t Int
  -- ^ Number of lines to scroll by
  , _scrollableConfig_scrollTo :: Event t ScrollPos
  -- ^ Specific position to scroll to
  , _scrollableConfig_startingPosition :: ScrollPos
  -- ^ The initial scroll position
  , _scrollableConfig_scrollToBottom :: Behavior t (Maybe ScrollToBottom)
  -- ^ How the scroll position should be adjusted as new content is added
  }

instance Reflex t => Default (ScrollableConfig t) where
  def = ScrollableConfig never never ScrollPos_Top (pure Nothing)

-- | The scroll position
data ScrollPos = ScrollPos_Top | ScrollPos_Line Int | ScrollPos_Bottom
  deriving (Show, Eq, Ord)

-- | The output of a 'scrollable', indicating its current scroll position.
data Scrollable t = Scrollable
  { _scrollable_scrollPosition :: Behavior t ScrollPos
  , _scrollable_totalLines :: Behavior t Int
  , _scrollable_scrollHeight :: Behavior t Int
  }

-- | Scrollable widget. The output exposes the current scroll position and
-- total number of lines (including those that are hidden)
scrollable
  :: forall t m. (Reflex t, MonadHold t m, MonadFix m, HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasTheme t m)
  => ScrollableConfig t
  -> (m (Behavior t V.Image, Event t ()))
  -> m (Scrollable t)
scrollable (ScrollableConfig scrollBy scrollTo startingPos onAppend) mkImg = do
  dw <- displayWidth
  (img, update) <- mkImg
  let sz = V.imageHeight <$> img
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
    [ (\((totalLines, h), d) sp -> Just $ scrollByLines sp totalLines h d) <$> attach ((,) <$> sz <*> current dh) requestedScroll
    , (\((totalLines, h), newScrollPosition) _ -> Just $ case newScrollPosition of
        ScrollPos_Line n -> scrollToLine totalLines h n
        ScrollPos_Top -> ScrollPos_Top
        ScrollPos_Bottom -> ScrollPos_Bottom
      ) <$> attach ((,) <$> sz <*> current dh) scrollTo
    , (\cfg sp -> case cfg of
        Just ScrollToBottom_Always -> case sp of
          ScrollPos_Bottom -> Nothing
          _ -> Just ScrollPos_Bottom
        _ -> Nothing) <$> tag onAppend update
    ]
  let imgsToTell height scrollPos totalLines images = case scrollPos of
        ScrollPos_Bottom -> V.translateY ((-1) * max 0 (totalLines - height)) images
        ScrollPos_Top -> images -- take height images
        ScrollPos_Line n -> V.translateY ((-1) * n) images
  tellImages $ fmap (:[]) $ imgsToTell <$> current dh <*> current lineIndex <*> sz <*> img
  return $ Scrollable
    { _scrollable_scrollPosition = current lineIndex
    , _scrollable_totalLines = sz
    , _scrollable_scrollHeight = current dh
    }

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
