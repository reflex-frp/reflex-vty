-- | Description: Widgets that scroll when their contents don't fit
module Reflex.Vty.Widget.Scroll where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Default
import Data.Functor (void)
import Data.List (foldl')
import qualified Graphics.Vty as V
import Reflex

import Reflex.Vty.Widget
import Reflex.Vty.Widget.Input.Mouse

-- | Configuration options for automatic scroll-to-bottom behavior
data ScrollToBottom
  = -- | Always scroll to the bottom on new output
    ScrollToBottom_Always
  | -- | Scroll down with new output only when, prior to the new output being
    --     added, the widget was scrolled all the way to the bottom.
    ScrollToBottom_Maintain
  deriving (Eq, Ord, Show)

-- | Controls when the scrollbar is visible.
data ScrollbarVisibility
  = -- | Always show gutter (track) + thumb when content overflows
    ScrollbarAlways
  | -- | Show thumb only (no gutter) when content overflows
    ScrollbarThumbOnly
  | -- | Show thumb only while actively scrolling; hides on the next
    -- non-scroll input. No gutter.
    ScrollbarWhileScrolling
  | -- | Never show scrollbar; child gets full width
    ScrollbarHidden
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
  , _scrollableConfig_scrollbarVisibility :: ScrollbarVisibility
  -- ^ When to show the scrollbar
  }

instance Reflex t => Default (ScrollableConfig t) where
  def = ScrollableConfig never never ScrollPos_Top (pure Nothing) ScrollbarThumbOnly

-- | The scroll position
data ScrollPos = ScrollPos_Top | ScrollPos_Line Int | ScrollPos_Bottom
  deriving (Eq, Ord, Show)

-- | The output of a 'scrollable', indicating its current scroll position.
data Scrollable t = Scrollable
  { _scrollable_scrollPosition :: Behavior t ScrollPos
  , _scrollable_totalLines :: Behavior t Int
  , _scrollable_scrollHeight :: Behavior t Int
  }

-- | Scrollable widget. The output exposes the current scroll position and
-- total number of lines (including those that are hidden)
scrollable
  :: forall t m a
   . ( Reflex t
     , MonadHold t m
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , HasDisplayRegion t m
     , HasInput t m
     , HasImageWriter t m
     , HasTheme t m
     )
  => ScrollableConfig t
  -> (m (Event t (), a))
  -> m (Scrollable t, a)
scrollable (ScrollableConfig scrollBy scrollTo startingPos onAppend sbVisibility) mkImg = do
  dh <- displayHeight
  dw <- displayWidth
  bt <- themeAttr
  kup <- key V.KUp
  kdown <- key V.KDown
  m <- mouseScroll
  let requestedScroll :: Event t Int
      requestedScroll =
        leftmost
          [ 1 <$ kdown
          , (-1) <$ kup
          , ffor m $ \case
              ScrollDirection_Up -> (-1)
              ScrollDirection_Down -> 1
          , scrollBy
          ]
      regionTransform = case sbVisibility of
        ScrollbarHidden -> id
        _ -> fmap shrinkRegionForScrollbar
  scrollingNow <- case sbVisibility of
    ScrollbarWhileScrolling -> do
      let scrollActivity = void requestedScroll
      hideAfterQuiet <- debounce 1.5 scrollActivity
      hold False $ leftmost [True <$ scrollActivity, False <$ hideAfterQuiet]
    _ -> pure (pure True)
  rec ((update, a), imgs) <- captureImages $ localRegion regionTransform $ localInput (translateMouseEvents translation) $ mkImg
      let sz = foldl' max 0 . fmap V.imageHeight <$> imgs
      lineIndex <-
        foldDynMaybe ($) startingPos $
          leftmost
            [ (\(totalLines, (h, d)) sp -> Just $ scrollByLines sp totalLines h d) <$> attach sz (attachPromptlyDyn dh requestedScroll)
            , ( \(totalLines, (h, newScrollPosition)) _ -> Just $ case newScrollPosition of
                  ScrollPos_Line n -> scrollToLine totalLines h n
                  ScrollPos_Top -> ScrollPos_Top
                  ScrollPos_Bottom -> ScrollPos_Bottom
              )
                <$> attach sz (attachPromptlyDyn dh scrollTo)
            , ( \cfg sp -> case cfg of
                  Just ScrollToBottom_Always -> case sp of
                    ScrollPos_Bottom -> Nothing
                    _ -> Just ScrollPos_Bottom
                  _ -> Nothing
              )
                <$> tag onAppend update
            ]
      let translation =
            calculateTranslation
              <$> current dh
              <*> current lineIndex
              <*> sz
  let cropImages dy images = cropFromTop dy <$> images
  tellImages $ cropImages <$> translation <*> imgs
  let sbImgs = scrollbarImages sbVisibility <$> bt <*> current dh <*> sz <*> translation <*> current dw
      sbGated = case sbVisibility of
        ScrollbarWhileScrolling ->
          (\vis imgs' -> if vis then imgs' else []) <$> scrollingNow <*> sbImgs
        _ -> sbImgs
  tellImages sbGated
  return $
    (,a) $
      Scrollable
        { _scrollable_scrollPosition = current lineIndex
        , _scrollable_totalLines = sz
        , _scrollable_scrollHeight = current dh
        }
  where
    shrinkRegionForScrollbar (Region l t w h) = Region l t (max 0 (w - 1)) h
    cropFromTop :: Int -> V.Image -> V.Image
    cropFromTop rows i =
      V.cropTop (max 0 $ V.imageHeight i - rows) i
    calculateTranslation height scrollPos totalLines = case scrollPos of
      ScrollPos_Bottom -> max 0 (totalLines - height)
      ScrollPos_Top -> 0
      ScrollPos_Line n -> max 0 n
    translateMouseEvents translation vtyEvent =
      let e = attach translation vtyEvent
      in ffor e $ \case
           (dy, V.EvMouseDown x y btn mods) -> V.EvMouseDown x (y + dy) btn mods
           (dy, V.EvMouseUp x y btn) -> V.EvMouseUp x (y + dy) btn
           (_, otherEvent) -> otherEvent

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
scrollToLine totalLines height newPos =
  if
    | totalLines <= height -> ScrollPos_Top
    | newPos == 0 -> ScrollPos_Top
    | newPos + height >= totalLines -> ScrollPos_Bottom
    | otherwise -> ScrollPos_Line newPos

-- | Build the scrollbar images (gutter + thumb). Returns empty list when
-- the scrollbar is hidden, the content fits within the viewport, or the
-- viewport is too narrow. In 'ScrollbarThumbOnly' and
-- 'ScrollbarWhileScrolling' modes, only the thumb is drawn (no gutter).
scrollbarImages
  :: ScrollbarVisibility
  -> V.Attr
  -- ^ Attribute for both gutter and thumb
  -> Int
  -- ^ Viewport height
  -> Int
  -- ^ Total content lines
  -> Int
  -- ^ Current scroll offset (lines scrolled from top)
  -> Int
  -- ^ Viewport width
  -> [V.Image]
scrollbarImages visibility attr vpHeight totalLines scrollLine vpWidth
  | visibility == ScrollbarHidden = []
  | totalLines <= vpHeight = []
  | vpWidth <= 1 = []
  | otherwise =
      let maxScroll = max 1 (totalLines - vpHeight)
          thumbLen = max 1 (vpHeight * vpHeight `div` totalLines)
          thumbTrack = vpHeight - thumbLen
          thumbTop = min thumbTrack (scrollLine * thumbTrack `div` maxScroll)
          gutterCol = vpWidth - 1
          thumbImg =
            withinImage (Region gutterCol thumbTop 1 thumbLen) $
              V.charFill attr '█' 1 thumbLen
          gutterImgs = case visibility of
            ScrollbarAlways ->
              [ withinImage (Region gutterCol 0 1 vpHeight) $
                  V.charFill attr '░' 1 vpHeight
              ]
            _ -> []
      in gutterImgs ++ [thumbImg]
