{-|
Module: Reflex.Vty.Widget
Description: Basic set of widgets and building blocks for reflex-vty applications
-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Vty.Widget
  ( VtyWidgetCtx(..)
  , VtyWidget(..)
  , VtyWidgetOut(..)
  , ImageWriter(..)
  , runVtyWidget
  , mainWidget
  , mainWidgetWithHandle
  , HasDisplaySize(..)
  , HasFocus(..)
  , HasVtyInput(..)
  , DynRegion(..)
  , currentRegion
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
  , KeyCombo
  , key
  , keys
  , keyCombos
  , blank
  , Orientation(..)
  , Constraint(..)
  , Layout
  , runLayout
  , tile
  , fixed
  , stretch
  , col
  , row
  , tabNavigation
  , askOrientation
  ) where

import Control.Applicative (liftA2)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class
import Control.Monad.Trans (lift)
import Control.Monad.Reader
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Default (Default(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid hiding (First(..))
import Data.Ratio
import Data.Semigroup (First(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import Graphics.Vty (Image)
import qualified Graphics.Vty as V
import Reflex
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Reflex.Class ()

import Reflex.Vty.Host

import Unsafe.NodeId

-- | The context within which a 'VtyWidget' runs
data VtyWidgetCtx t = VtyWidgetCtx
  { _vtyWidgetCtx_width :: Dynamic t Int
    -- ^ The width of the region allocated to the widget.
  , _vtyWidgetCtx_height :: Dynamic t Int
    -- ^ The height of the region allocated to the widget.
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
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadSample t
    , MonadHold t
    , MonadFix
    , NotReady t
    , ImageWriter t
    , PostBuild t
    , TriggerEvent t
    , MonadReflexCreateTrigger t
    )

deriving instance PerformEvent t m => PerformEvent t (VtyWidget t m)
instance MonadTrans (VtyWidget t) where
  lift f = VtyWidget $ lift $ lift f

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
          { _vtyWidgetCtx_width = fmap fst size
          , _vtyWidgetCtx_height = fmap snd size
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
  -- | Retrieve the display width (columns)
  displayWidth :: m (Dynamic t Int)
  default displayWidth :: (f m' ~ m, MonadTrans f, HasDisplaySize t m') => m (Dynamic t Int)
  displayWidth = lift displayWidth
  -- | Retrieve the display height (rows)
  displayHeight :: m (Dynamic t Int)
  default displayHeight :: (f m' ~ m, MonadTrans f, HasDisplaySize t m') => m (Dynamic t Int)
  displayHeight = lift displayHeight

instance (Reflex t, Monad m) => HasDisplaySize t (VtyWidget t m) where
  displayWidth = VtyWidget . lift $ asks _vtyWidgetCtx_width
  displayHeight = VtyWidget . lift $ asks _vtyWidgetCtx_height

instance HasDisplaySize t m => HasDisplaySize t (ReaderT x m)
instance HasDisplaySize t m => HasDisplaySize t (BehaviorWriterT t x m)
instance HasDisplaySize t m => HasDisplaySize t (DynamicWriterT t x m)
instance HasDisplaySize t m => HasDisplaySize t (EventWriterT t x m)

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

-- | A dynamic chunk of the display area
data DynRegion t = DynRegion
  { _dynRegion_left :: Dynamic t Int
  , _dynRegion_top :: Dynamic t Int
  , _dynRegion_width :: Dynamic t Int
  , _dynRegion_height :: Dynamic t Int
  }

switchDynRegion :: Reflex t => Dynamic t (DynRegion t) -> DynRegion t
switchDynRegion r = DynRegion
  { _dynRegion_left = _dynRegion_left =<< r
  , _dynRegion_top = _dynRegion_top =<< r
  , _dynRegion_width = _dynRegion_width =<< r
  , _dynRegion_height = _dynRegion_height =<< r
  }

-- | The width and height of a 'Region'
regionSize :: Region -> (Int, Int)
regionSize (Region _ _ w h) = (w, h)

-- | Produces an 'Image' that fills a region with space characters
regionBlankImage :: Region -> Image
regionBlankImage r@(Region _ _ width height) =
  withinImage r $ V.charFill V.defAttr ' ' width height

-- | A behavior of the current display area represented by a 'DynRegion'
currentRegion :: Reflex t => DynRegion t -> Behavior t Region
currentRegion (DynRegion l t w h) = Region <$> current l <*> current t <*> current w <*> current h

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
  => DynRegion t
  -> Dynamic t Bool -- ^ Whether the widget should be focused when the parent is.
  -> VtyWidget t m a
  -> VtyWidget t m a
pane dr foc child = VtyWidget $ do
  ctx <- lift ask
  let reg = currentRegion dr
  let ctx' = VtyWidgetCtx
        { _vtyWidgetCtx_input = leftmost -- TODO: think about this leftmost more.
            [ fmapMaybe id $
                attachWith (\(r,f) e -> filterInput r f e)
                  (liftA2 (,) reg (current foc))
                  (_vtyWidgetCtx_input ctx)
            ]
        , _vtyWidgetCtx_focus = liftA2 (&&) (_vtyWidgetCtx_focus ctx) foc
        , _vtyWidgetCtx_width = _dynRegion_width dr
        , _vtyWidgetCtx_height = _dynRegion_height dr
        }
  (result, images) <- lift . lift $ runVtyWidget ctx' child
  let images' = liftA2 (\r is -> map (withinImage r) is) reg images
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

-- | Type synonym for a key and modifier combination
type KeyCombo = (V.Key, [V.Modifier])

-- | Emits an event that fires on a particular key press (without modifiers)
key :: (Monad m, Reflex t) => V.Key -> VtyWidget t m (Event t KeyCombo)
key = keyCombos . Set.singleton . (,[])

-- | Emits an event that fires on particular key presses (without modifiers)
keys :: (Monad m, Reflex t) => [V.Key] -> VtyWidget t m (Event t KeyCombo)
keys = keyCombos . Set.fromList . fmap (,[])

-- | Emit an event that fires whenever any of the provided key combinations occur
keyCombos
  :: (Reflex t, Monad m)
  => Set KeyCombo
  -> VtyWidget t m (Event t KeyCombo)
keyCombos ks = do
  i <- input
  return $ fforMaybe i $ \case
    V.EvKey k m -> if Set.member (k, m) ks
      then Just (k, m)
      else Nothing
    _ -> Nothing

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
  dw <- displayWidth
  dh <- displayHeight
  let regA = DynRegion
        { _dynRegion_left = pure 0
        , _dynRegion_top = pure 0
        , _dynRegion_width = dw
        , _dynRegion_height = sizeFunD <*> dh
        }
      regB = DynRegion
        { _dynRegion_left = pure 0
        , _dynRegion_top = _dynRegion_height regA
        , _dynRegion_width = dw
        , _dynRegion_height = liftA2 (-) dh (_dynRegion_height regA)
        }
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
  dh <- displayHeight
  dw <- displayWidth
  h0 <- sample $ current dh -- TODO
  dragE <- drag V.BLeft
  let splitter0 = h0 `div` 2
  rec splitterCheckpoint <- holdDyn splitter0 $ leftmost [fst <$> ffilter snd dragSplitter, resizeSplitter]
      splitterPos <- holdDyn splitter0 $ leftmost [fst <$> dragSplitter, resizeSplitter]
      splitterFrac <- holdDyn ((1::Double) / 2) $ ffor (attach (current dh) (fst <$> dragSplitter)) $ \(h, x) ->
        fromIntegral x / fromIntegral h
      let dragSplitter = fforMaybe (attach (current splitterCheckpoint) dragE) $
            \(splitterY, Drag (_, fromY) (_, toY) _ _ end) ->
              if splitterY == fromY then Just (toY, end) else Nothing
          regA = DynRegion 0 0 dw splitterPos
          regS = DynRegion 0 splitterPos dw 1
          regB = DynRegion 0 (splitterPos + 1) dw (dh - splitterPos - 1)
          resizeSplitter = ffor (attach (current splitterFrac) (updated dh)) $
            \(frac, h) -> round (frac * fromIntegral h)
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
  dw <- displayWidth
  dh <- displayHeight
  let fillImg = current $ liftA2 (\w h -> [V.charFill V.defAttr c w h]) dw dh
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
  dh <- displayHeight
  dw <- displayWidth
  let boxReg = DynRegion (pure 0) (pure 0) dw dh
      innerReg = DynRegion (pure 1) (pure 1) (subtract 2 <$> dw) (subtract 2 <$> dh)
  tellImages (boxImages <$> boxStyle <*> currentRegion boxReg)
  tellImages (fmap (\r -> [regionBlankImage r]) (currentRegion innerReg))
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

data Constraint = Constraint_Fixed Int
                | Constraint_Min Int
  deriving (Show, Read, Eq, Ord)

-- | Compute the size of each widget "@k@" based on the total set of 'Constraint's
computeSizes
  :: Ord k
  => Int
  -> Map k (a, Constraint)
  -> Map k (a, Int)
computeSizes available constraints =
  let minTotal = sum $ ffor (Map.elems constraints) $ \case
        (_, Constraint_Fixed n) -> n
        (_, Constraint_Min n) -> n
      leftover = max 0 (available - minTotal)
      numStretch = Map.size $ Map.filter (isMin . snd) constraints
      szStretch = floor $ leftover % numStretch
      adjustment = max 0 $ available - minTotal - szStretch * numStretch
  in snd $ Map.mapAccum (\adj (a, c) -> case c of
      Constraint_Fixed n -> (adj, (a, n))
      Constraint_Min n -> (0, (a, n + szStretch + adj))) adjustment constraints
  where
    isMin (Constraint_Min _) = True
    isMin _ = False

computeEdges :: (Ord k) => Map k (a, Int) -> Map k (a, (Int, Int))
computeEdges = fst . Map.foldlWithKey' (\(m, offset) k (a, sz) ->
  (Map.insert k (a, (offset, sz)) m, sz + offset)) (Map.empty, 0)

blank :: Monad m => VtyWidget t m ()
blank = return ()

-- | The main-axis orientation of a 'StackWidget' widget
data Orientation = Orientation_Column
                 | Orientation_Row
  deriving (Show, Read, Eq, Ord)

data LayoutCtx t = LayoutCtx
  { _layoutCtx_regions :: Dynamic t (Map NodeId (DynRegion t))
  , _layoutCtx_focusDemux :: Demux t (Maybe NodeId)
  , _layoutCtx_orientation :: Dynamic t Orientation
  }

newtype Layout t m a = Layout
  { unLayout :: EventWriterT t (First NodeId)
      (DynamicWriterT t (Endo [(NodeId, (Bool, Constraint))])
        (ReaderT (LayoutCtx t)
          (VtyWidget t m))) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadHold t
    , MonadSample t
    , MonadFix
    , TriggerEvent t
    , PerformEvent t
    , NotReady t
    , MonadReflexCreateTrigger t
    )

instance MonadTrans (Layout t) where
  lift x = Layout $ lift $ lift $ lift $ lift x

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (Layout t m) where
  runWithReplace (Layout a) e = Layout $ runWithReplace a $ fmap unLayout e
  traverseIntMapWithKeyWithAdjust f m e = Layout $ traverseIntMapWithKeyWithAdjust (\k v -> unLayout $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = Layout $ traverseDMapWithKeyWithAdjust (\k v -> unLayout $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = Layout $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unLayout $ f k v) m e

runLayout
  :: (MonadFix m, MonadHold t m, PostBuild t m)
  => Dynamic t Orientation
  -> Int
  -> Event t Int
  -> Layout t m a
  -> VtyWidget t m a
runLayout ddir focus0 focusShift (Layout child) = do
  dw <- displayWidth
  dh <- displayHeight
  let main = ffor3 ddir dw dh $ \d w h -> case d of
        Orientation_Column -> h
        Orientation_Row -> w
  pb <- getPostBuild
  rec ((a, focusReq), queriesEndo) <- runReaderT (runDynamicWriterT $ runEventWriterT child) $ LayoutCtx solutionMap focusDemux ddir
      let queries = flip appEndo [] <$> queriesEndo
          solution = ffor2 main queries $ \sz qs -> Map.fromList
            . Map.elems
            . computeEdges
            . computeSizes sz
            . fmap (fmap snd)
            . Map.fromList
            . zip [0::Integer ..]
            $ qs
          solutionMap = ffor solution $ \ss -> ffor ss $ \(offset, sz) -> DynRegion
            { _dynRegion_left = ffor ddir $ \case
              Orientation_Column -> 0
              Orientation_Row -> offset
            , _dynRegion_width = ffor2 dw ddir $ \w -> \case
              Orientation_Column -> w
              Orientation_Row -> sz
            , _dynRegion_top = ffor ddir $ \case
              Orientation_Column -> offset
              Orientation_Row -> 0
            , _dynRegion_height = ffor2 dh ddir $ \h -> \case
              Orientation_Column -> sz
              Orientation_Row -> h
            }
          focusable = fmap (Bimap.fromList . zip [0..]) $
            ffor queries $ \qs -> fforMaybe qs $ \(nodeId, (f, _)) ->
              if f then Just nodeId else Nothing
          adjustFocus
            :: (Bimap Int NodeId, (Int, Maybe NodeId))
            -> Either Int NodeId
            -> (Int, Maybe NodeId)
          adjustFocus (fm, (cur, _)) (Left shift) =
            let ix = (cur + shift) `mod` Bimap.size fm
            in (ix, Bimap.lookup ix fm)
          adjustFocus (fm, (cur, _)) (Right goto) =
            let ix = fromMaybe cur $ Bimap.lookupR goto fm
            in (ix, Just goto)
          focusChange = attachWith
            adjustFocus
            (current $ (,) <$> focusable <*> focussed)
            $ leftmost [Left <$> focusShift, Left 0 <$ pb, Right . getFirst <$> focusReq]
      -- A pair (Int, Maybe NodeId) which represents the index
      -- that we're trying to focus, and the node that actually gets
      -- focused (at that index) if it exists
      focussed <- holdDyn (focus0, Nothing) focusChange
      let focusDemux = demux $ snd <$> focussed
  return a

tile
  :: (Reflex t, Monad m)
  => Dynamic t Constraint
  -> Dynamic t Bool
  -> VtyWidget t m (Event t x, a)
  -> Layout t m a
tile con focusable child = do
  let nodeId = unsafeNodeId child
  Layout $ tellDyn $ ffor2 con focusable $ \c f -> Endo ((nodeId, (f, c)):)
  reg <- fmap switchDynRegion $ Layout $ asks $
    fmap (Map.findWithDefault (error "tile: Could not find solution for nodeId. This should be impossible.") nodeId) . _layoutCtx_regions
  focussed <- Layout $ asks _layoutCtx_focusDemux
  (focusReq, a) <- Layout $ lift $ lift $ lift $
    pane reg (demuxed focussed $ Just nodeId) child
  Layout $ tellEvent $ First nodeId <$ focusReq
  return a

data TileConfig t = TileConfig
  { _tileConfig_constraint :: Dynamic t Constraint
  , _tileConfig_focusable :: Dynamic t Bool
  , _tileConfig_clickable :: Behavior t Bool
  }

instance Reflex t => Default (TileConfig t) where
  def = TileConfig (pure $ Constraint_Min 0) (pure True) (pure True)

fixed
  :: (Reflex t, Monad m)
  => Dynamic t Int
  -> VtyWidget t m a
  -> Layout t m a
fixed sz = tile (Constraint_Fixed <$> sz) (pure True) . clickable

stretch
  :: (Reflex t, Monad m)
  => VtyWidget t m a
  -> Layout t m a
stretch = tile (Constraint_Min <$> 0) (pure True) . clickable

col
  :: (MonadFix m, MonadHold t m, PostBuild t m)
  => Layout t m a
  -> VtyWidget t m a
col child = do
  nav <- tabNavigation
  runLayout (pure Orientation_Column) 0 nav child

row
  :: (MonadFix m, MonadHold t m, PostBuild t m)
  => Layout t m a
  -> VtyWidget t m a
row child = do
  nav <- tabNavigation
  runLayout (pure Orientation_Column) 0 nav child

tabNavigation :: (Reflex t, Monad m) => VtyWidget t m (Event t Int)
tabNavigation = do
  fwd <- fmap (const 1) <$> key (V.KChar '\t')
  back <- fmap (const (-1)) <$> key V.KBackTab
  return $ leftmost [fwd, back]

clickable
  :: (Reflex t, Monad m)
  => VtyWidget t m a
  -> VtyWidget t m (Event t (), a)
clickable child = do
  click <- mouseDown V.BLeft
  a <- child
  return (() <$ click, a)

askOrientation :: Monad m => Layout t m (Dynamic t Orientation)
askOrientation = Layout $ asks _layoutCtx_orientation
