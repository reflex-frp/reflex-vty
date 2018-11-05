{-|
Module: Reflex.Vty.Widget
Description: Basic set of widgets and building blocks for reflex-vty applications
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Vty.Widget
  ( VtyWidgetCtx(..)
  , VtyWidget(..)
  , VtyWidgetOut(..)
  , runVtyWidget
  , mainWidget
  , mainWidgetWithHandle
  , HasDisplaySize(..)
  , displayWidth
  , displayHeight
  , HasFocus(..)
  , HasVtyInput(..)
  , Region(..)
  , regionSize
  , regionBlankImage
  , Drag(..)
  , drag
  , MouseDown(..)
  , MouseUp(..)
  , mouseDown
  , mouseUp
  , pane
  , tellImages
  , splitV
  , splitVDrag
  , box
  , boxStatic
  , RichTextConfig(..)
  , richText
  , text
  , display
  , BoxStyle(..)
  , hyphenBoxStyle
  , singleBoxStyle
  , roundedBoxStyle
  , thickBoxStyle
  , doubleBoxStyle
  , fill
  , hRule
  ) where

import Control.Applicative (liftA2)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks, ask)
import Data.Default (Default(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import Graphics.Vty (Image)
import qualified Graphics.Vty as V
import Reflex
import Reflex.NotReady.Class
import Reflex.NotReady.Class.Orphans ()
import Reflex.BehaviorWriter.Class (tellBehavior)
import Reflex.BehaviorWriter.Base (BehaviorWriterT, runBehaviorWriterT)

import Reflex.Vty.Host

-- | The context within which a 'VtyWidget' runs
data VtyWidgetCtx t = VtyWidgetCtx
  { _vtyWidgetCtx_size :: Dynamic t (Int,Int)
    -- ^ The width and height of the region allocated to the widget.
  , _vtyWidgetCtx_focus :: Dynamic t Bool
    -- ^ Whether the widget should behave as if it has focus for keyboard input.
  , _vtyWidgetCtx_input :: Event t VtyEvent
    -- ^ User input events that the widget's parent chooses to share. These will generally
    -- be filtered for relevance:
    --  * Keyboard inputs are restricted to focused widgets
    --  * Mouse inputs are restricted to the region in which the widget resides and are
    --  translated into its internal coordinates.
  }

-- | The output of a 'VtyWidget'
data VtyWidgetOut t = VtyWidgetOut
  { _vtyWidgetOut_shutdown :: Event t ()
  }

instance (Adjustable t m, MonadHold t m, Reflex t) => Adjustable t (VtyWidget t m) where
  runWithReplace a0 a' = VtyWidget $ runWithReplace (unVtyWidget a0) $ fmap unVtyWidget a'
  traverseIntMapWithKeyWithAdjust f dm0 dm' = VtyWidget $
    traverseIntMapWithKeyWithAdjust (\k v -> unVtyWidget (f k v)) dm0 dm'
  traverseDMapWithKeyWithAdjust f dm0 dm' = VtyWidget $ do
    traverseDMapWithKeyWithAdjust (\k v -> unVtyWidget (f k v)) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = VtyWidget $ do
    traverseDMapWithKeyWithAdjustWithMove (\k v -> unVtyWidget (f k v)) dm0 dm'

-- | A widget that can read its context and produce image output
newtype VtyWidget t m a = VtyWidget
  { unVtyWidget :: BehaviorWriterT t [Image] (ReaderT (VtyWidgetCtx t) m) a
  }
  deriving (Functor, Applicative, Monad, MonadSample t, MonadHold t, MonadFix, NotReady t, ImageWriter t)

-- | Runs a 'VtyWidget' with a given context
runVtyWidget
  :: (Reflex t, Monad m)
  => VtyWidgetCtx t
  -> VtyWidget t m a
  -> m (a, Behavior t [Image])
runVtyWidget ctx w = runReaderT (runBehaviorWriterT (unVtyWidget w)) ctx

-- | Sets up the top-level context for a 'VtyWidget' and runs it with that context
mainWidgetWithHandle
  :: V.Vty
  -> (forall t m. MonadVtyApp t m => VtyWidget t m (Event t ()))
  -> IO ()
mainWidgetWithHandle vty child =
  runVtyAppWithHandle vty $ \dr0 inp -> do
    size <- holdDyn dr0 $ fforMaybe inp $ \case
      V.EvResize w h -> Just (w, h)
      _ -> Nothing
    let inp' = fforMaybe inp $ \case
          V.EvResize {} -> Nothing
          x -> Just x
    let ctx = VtyWidgetCtx
          { _vtyWidgetCtx_size = size
          , _vtyWidgetCtx_input = inp'
          , _vtyWidgetCtx_focus = constDyn True
          }
    (shutdown, images) <- runVtyWidget ctx $ do
      tellImages . ffor (current size) $ \(w, h) -> [V.charFill V.defAttr ' ' w h]
      child
    return $ VtyResult
      { _vtyResult_picture = fmap (V.picForLayers . reverse) images
      , _vtyResult_shutdown = shutdown
      }

-- | Like 'mainWidgetWithHandle', but uses a default vty configuration
mainWidget
  :: (forall t m. MonadVtyApp t m => VtyWidget t m (Event t ()))
  -> IO ()
mainWidget child = do
  vty <- getDefaultVty
  mainWidgetWithHandle vty child

-- | A class for things that know their own display size dimensions
class (Reflex t, Monad m) => HasDisplaySize t m | m -> t where
  displaySize :: m (Dynamic t (Int, Int))

instance (Reflex t, Monad m) => HasDisplaySize t (VtyWidget t m) where
  displaySize = VtyWidget . lift $ asks _vtyWidgetCtx_size

-- | Retrieve the display width (columns)
displayWidth :: HasDisplaySize t m => m (Dynamic t Int)
displayWidth = fmap fst <$> displaySize

-- | Retrieve the display height (rows)
displayHeight :: HasDisplaySize t m => m (Dynamic t Int)
displayHeight = fmap snd <$> displaySize

-- | A class for things that can receive vty events as input
class HasVtyInput t m | m -> t where
  input :: m (Event t VtyEvent)

instance (Reflex t, Monad m) => HasVtyInput t (VtyWidget t m) where
  input = VtyWidget . lift $ asks _vtyWidgetCtx_input

-- | A class for things that can dynamically gain and lose focus
class HasFocus t m | m -> t where
  focus :: m (Dynamic t Bool)

instance (Reflex t, Monad m) => HasFocus t (VtyWidget t m) where
  focus = VtyWidget . lift $ asks _vtyWidgetCtx_focus

-- | A class for widgets that can produce images to draw to the display
class (Reflex t, Monad m) => ImageWriter t m | m -> t where
  -- | Send images upstream for rendering
  tellImages :: Behavior t [Image] -> m ()

instance (Monad m, Reflex t) => ImageWriter t (BehaviorWriterT t [Image] m) where
  tellImages = tellBehavior

-- | A chunk of the display area
data Region = Region
  { _region_left :: Int
  , _region_top :: Int
  , _region_width :: Int
  , _region_height :: Int
  }
  deriving (Show, Read, Eq, Ord)

-- | The width and height of a 'Region'
regionSize :: Region -> (Int, Int)
regionSize (Region _ _ w h) = (w, h)

-- | Produces an 'Image' that fills a region with space characters
regionBlankImage :: Region -> Image
regionBlankImage r@(Region _ _ width height) =
  withinImage r $ V.charFill V.defAttr ' ' width height

-- | Translates and crops an 'Image' so that it is contained by
-- the given 'Region'.
withinImage
  :: Region
  -> Image
  -> Image
withinImage (Region left top width height)
  | width < 0 || height < 0 = withinImage (Region left top 0 0)
  | otherwise = V.translate left top . V.crop width height

-- | Low-level widget combinator that runs a child 'VtyWidget' within
-- a given region and context. This widget filters and modifies the input
-- that the child widget receives such that:
-- * unfocused widgets receive no key events
-- * mouse inputs outside the region are ignored
-- * mouse inputs inside the region have their coordinates translated such
--   that (0,0) is the top-left corner of the region
pane
  :: (Reflex t, Monad m)
  => Dynamic t Region -- ^ Region into which we should draw the widget (in coordinates relative to our own)
  -> Dynamic t Bool -- ^ Whether the widget should be focused when the parent is.
  -> VtyWidget t m a
  -> VtyWidget t m a
pane reg foc child = VtyWidget $ do
  ctx <- lift ask
  let ctx' = VtyWidgetCtx
        { _vtyWidgetCtx_input = leftmost -- TODO: think about this leftmost more.
            [ fmapMaybe id $
                attachWith (\(r,f) e -> filterInput r f e)
                  (liftA2 (,) (current reg) (current foc))
                  (_vtyWidgetCtx_input ctx)
            ]
        , _vtyWidgetCtx_focus = liftA2 (&&) (_vtyWidgetCtx_focus ctx) foc
        , _vtyWidgetCtx_size = fmap regionSize reg }
  (result, images) <- lift . lift $ runVtyWidget ctx' child
  let images' = liftA2 (\r is -> map (withinImage r) is) (current reg) images
  tellImages images'
  return result
  where
    filterInput :: Region -> Bool -> VtyEvent -> Maybe VtyEvent
    filterInput (Region l t w h) focused e = case e of
      V.EvKey _ _ | not focused -> Nothing
      V.EvMouseDown x y btn m -> mouse (\u v -> V.EvMouseDown u v btn m) x y
      V.EvMouseUp x y btn -> mouse (\u v -> V.EvMouseUp u v btn) x y
      _ -> Just e
      where
        mouse con x y
          | or [ x < l
               , y < t
               , x >= l + w
               , y >= t + h ] = Nothing
          | otherwise =
            Just (con (x - l) (y - t))

-- | Information about a drag operation
data Drag = Drag
  { _drag_from :: (Int, Int) -- ^ Where the drag began
  , _drag_to :: (Int, Int) -- ^ Where the mouse currently is
  , _drag_button :: V.Button -- ^ Which mouse button is dragging
  , _drag_modifiers :: [V.Modifier] -- ^ What modifiers are held
  , _drag_end :: Bool -- ^ Whether the drag ended (the mouse button was released)
  }
  deriving (Eq, Ord, Show)

-- | Converts raw vty mouse drag events into an event stream of 'Drag's
drag
  :: (Reflex t, MonadFix m, MonadHold t m)
  => V.Button
  -> VtyWidget t m (Event t Drag)
drag btn = do
  inp <- input
  let f :: Maybe Drag -> V.Event -> Maybe Drag
      f Nothing = \case
        V.EvMouseDown x y btn' mods
          | btn == btn' -> Just $ Drag (x,y) (x,y) btn' mods False
          | otherwise -> Nothing
        _ -> Nothing
      f (Just (Drag from _ _ mods end)) = \case
        V.EvMouseDown x y btn' mods'
          | end         -> Just $ Drag (x,y) (x,y) btn' mods' False
          | btn == btn' -> Just $ Drag from (x,y) btn mods' False
          | otherwise   -> Nothing -- Ignore other buttons.
        V.EvMouseUp x y (Just btn')
          | end         -> Nothing
          | btn == btn' -> Just $ Drag from (x,y) btn mods True
          | otherwise   -> Nothing
        V.EvMouseUp x y Nothing -- Terminal doesn't specify mouse up button,
                                -- assume it's the right one.
          | end       -> Nothing
          | otherwise -> Just $ Drag from (x,y) btn mods True
        _ -> Nothing
  rec let newDrag = attachWithMaybe f (current dragD) inp
      dragD <- holdDyn Nothing $ Just <$> newDrag
  return (fmapMaybe id $ updated dragD)

-- | Mouse down events for a particular mouse button
mouseDown
  :: (Reflex t, Monad m)
  => V.Button
  -> VtyWidget t m (Event t MouseDown)
mouseDown btn = do
  i <- input
  return $ fforMaybe i $ \case
    V.EvMouseDown x y btn' mods -> if btn == btn'
      then Just $ MouseDown btn' (x, y) mods
      else Nothing
    _ -> Nothing

-- | Mouse up events for a particular mouse button
mouseUp
  :: (Reflex t, Monad m)
  => VtyWidget t m (Event t MouseUp)
mouseUp = do
  i <- input
  return $ fforMaybe i $ \case
    V.EvMouseUp x y btn' -> Just $ MouseUp btn' (x, y)
    _ -> Nothing

-- | Information about a mouse down event
data MouseDown = MouseDown
  { _mouseDown_button :: V.Button
  , _mouseDown_coordinates :: (Int, Int)
  , _mouseDown_modifiers :: [V.Modifier]
  }
  deriving (Eq, Ord, Show)

-- | Information about a mouse up event
data MouseUp = MouseUp
  { _mouseUp_button :: Maybe V.Button
  , _mouseUp_coordinates :: (Int, Int)
  }
  deriving (Eq, Ord, Show)

-- | A plain split of the available space into vertically stacked panes.
-- No visual separator is built in here.
splitV :: (Reflex t, Monad m)
       => Dynamic t (Int -> Int)
       -- ^ Function used to determine size of first pane based on available size
       -> Dynamic t (Bool, Bool)
       -- ^ How to focus the two sub-panes, given that we are focused.
       -> VtyWidget t m a
       -- ^ Widget for first pane
       -> VtyWidget t m b
       -- ^ Widget for second pane
       -> VtyWidget t m (a,b)
splitV sizeFunD focD wA wB = do
  sz <- displaySize
  let regA = (\f (w,h) -> Region 0 0 w (f h)) <$> sizeFunD <*> sz
      regB = (\(w,h) (Region _ _ _ hA) -> Region 0 hA w (h - hA)) <$> sz <*> regA
  ra <- pane regA (fst <$> focD) wA
  rb <- pane regB (snd <$> focD) wB
  return (ra,rb)

-- | A split of the available space into two parts with a draggable separator.
-- Starts with half the space allocated to each, and the first pane has focus.
-- Clicking in a pane switches focus.
splitVDrag :: (Reflex t, MonadFix m, MonadHold t m)
  => VtyWidget t m ()
  -> VtyWidget t m a
  -> VtyWidget t m b
  -> VtyWidget t m (a,b)
splitVDrag wS wA wB = do
  sz <- displaySize
  (_, h0) <- sample $ current sz
  dragE <- drag V.BLeft
  let splitter0 = h0 `div` 2
  rec splitterCheckpoint <- holdDyn splitter0 $ leftmost [fst <$> ffilter snd dragSplitter, resizeSplitter]
      splitterPos <- holdDyn splitter0 $ leftmost [fst <$> dragSplitter, resizeSplitter]
      splitterFrac <- holdDyn ((1::Double) / 2) $ ffor (attach (current sz) (fst <$> dragSplitter)) $ \((_,h),x) ->
        fromIntegral x / fromIntegral h
      let dragSplitter = fforMaybe (attach (current splitterCheckpoint) dragE) $
            \(splitterY, Drag (_, fromY) (_, toY) _ _ end) ->
              if splitterY == fromY then Just (toY, end) else Nothing
          regA = (\(w,_) sp -> Region 0 0 w sp) <$> sz <*> splitterPos
          regS = (\(w,_) sp -> Region 0 sp w 1) <$> sz <*> splitterPos
          regB = (\(w,h) sp -> Region 0 (sp + 1) w (h - sp - 1)) <$> sz <*> splitterPos
          resizeSplitter = ffor (attach (current splitterFrac) (updated sz)) $
            \(frac, (_,h)) -> round (frac * fromIntegral h)
      focA <- holdDyn True $ leftmost
        [ True <$ mA
        , False <$ mB
        ]
      (mA, rA) <- pane regA focA $ withMouseDown wA
      pane regS (pure False) wS
      (mB, rB) <- pane regB (not <$> focA) $ withMouseDown wB
  return (rA, rB)
  where
    withMouseDown x = do
      m <- mouseDown V.BLeft
      x' <- x
      return (m, x')

-- | Fill the background with a particular character.
fill :: (Reflex t, Monad m) => Char -> VtyWidget t m ()
fill c = do
  sz <- displaySize
  let fillImg = ffor (current sz) $ \(w,h) -> [V.charFill V.defAttr c w h]
  tellImages fillImg

-- | Fill the background with the bottom
hRule :: (Reflex t, Monad m) => BoxStyle -> VtyWidget t m ()
hRule boxStyle = fill (_boxStyle_s boxStyle)

-- | Defines a set of symbols to use to draw the outlines of boxes
-- C.f. https://en.wikipedia.org/wiki/Box-drawing_character
data BoxStyle = BoxStyle
  { _boxStyle_nw :: Char
  , _boxStyle_n :: Char
  , _boxStyle_ne :: Char
  , _boxStyle_e :: Char
  , _boxStyle_se :: Char
  , _boxStyle_s :: Char
  , _boxStyle_sw :: Char
  , _boxStyle_w :: Char
  }

instance Default BoxStyle where
  def = singleBoxStyle

-- | A box style that uses hyphens and pipe characters. Doesn't handle
-- corners very well.
hyphenBoxStyle :: BoxStyle
hyphenBoxStyle = BoxStyle '-' '-' '-' '|' '-' '-' '-' '|'

-- | A single line box style
singleBoxStyle :: BoxStyle
singleBoxStyle = BoxStyle '┌' '─' '┐' '│' '┘' '─' '└' '│'

-- | A thick single line box style
thickBoxStyle :: BoxStyle
thickBoxStyle = BoxStyle '┏' '━' '┓' '┃' '┛' '━' '┗' '┃'

-- | A double line box style
doubleBoxStyle :: BoxStyle
doubleBoxStyle = BoxStyle '╔' '═' '╗' '║' '╝' '═' '╚' '║'

-- | A single line box style with rounded corners
roundedBoxStyle :: BoxStyle
roundedBoxStyle = BoxStyle '╭' '─' '╮' '│' '╯' '─' '╰' '│'

-- | Draws a box in the provided style and a child widget inside of that box
box :: (Monad m, Reflex t)
    => Behavior t BoxStyle
    -> VtyWidget t m a
    -> VtyWidget t m a
box boxStyle child = do
  sz <- displaySize
  let boxReg = ffor (current sz) $ \(w,h) -> Region 0 0 w h
      innerReg = ffor sz $ \(w,h) -> Region 1 1 (w - 2) (h - 2)
  tellImages (boxImages <$> boxStyle <*> boxReg)
  tellImages (fmap (\r -> [regionBlankImage r]) (current innerReg))
  pane innerReg (pure True) child
  where
    boxImages :: BoxStyle -> Region -> [Image]
    boxImages style (Region left top width height) =
      let right = left + width - 1
          bottom = top + height - 1
          sides =
            [ withinImage (Region (left + 1) top (width - 2) 1) $
                V.charFill V.defAttr (_boxStyle_n style) (width - 2) 1
            , withinImage (Region right (top + 1) 1 (height - 2)) $
                V.charFill V.defAttr (_boxStyle_e style) 1 (height - 2)
            , withinImage (Region (left + 1) bottom (width - 2) 1) $
                V.charFill V.defAttr (_boxStyle_s style) (width - 2) 1
            , withinImage (Region left (top + 1) 1 (height - 2)) $
                V.charFill V.defAttr (_boxStyle_w style) 1 (height - 2)
            ]
          corners =
            [ withinImage (Region left top 1 1) $
                V.char V.defAttr (_boxStyle_nw style)
            , withinImage (Region right top 1 1) $
                V.char V.defAttr (_boxStyle_ne style)
            , withinImage (Region right bottom 1 1) $
                V.char V.defAttr (_boxStyle_se style)
            , withinImage (Region left bottom 1 1) $
                V.char V.defAttr (_boxStyle_sw style)
            ]
      in sides ++ if width > 1 && height > 1 then corners else []

-- | A box whose style is static
boxStatic
  :: (Reflex t, Monad m)
  => BoxStyle
  -> VtyWidget t m a
  -> VtyWidget t m a
boxStatic = box . pure

-- | Configuration options for displaying "rich" text
data RichTextConfig t = RichTextConfig
  { _richTextConfig_attributes :: Behavior t V.Attr
  }

instance Reflex t => Default (RichTextConfig t) where
  def = RichTextConfig $ pure V.defAttr

-- | A widget that displays text with custom time-varying attributes
richText
  :: (Reflex t, Monad m)
  => RichTextConfig t
  -> Behavior t Text
  -> VtyWidget t m ()
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
  :: (Reflex t, Monad m)
  => Behavior t Text
  -> VtyWidget t m ()
text = richText def

-- | Renders any behavior whose value can be converted to
-- 'String' as text
display
  :: (Reflex t, Monad m, Show a)
  => Behavior t a
  -> VtyWidget t m ()
display a = text $ T.pack . show <$> a
