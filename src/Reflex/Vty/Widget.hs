{-|
Module: Reflex.Vty.Widget
Description: Basic set of widgets and building blocks for reflex-vty applications
-}
{-# Language UndecidableInstances #-}

module Reflex.Vty.Widget where

import Control.Applicative (liftA2)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph
import Control.Monad.NodeId
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Ref
import Control.Monad.Trans (MonadTrans, lift)
import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.Vty (Image)
import qualified Graphics.Vty as V
import Reflex
import Reflex.Class ()
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Reflex.Vty.Host

-- * Running a vty application

-- | Sets up the top-level context for a vty widget and runs it with that context
mainWidgetWithHandle
  :: V.Vty
  -> (forall t m.
      ( MonadVtyApp t m
      , HasImageWriter t m
      , MonadNodeId m
      , HasDisplayRegion t m
      , HasFocusReader t m
      , HasInput t m
      ) => m (Event t ()))
  -> IO ()
mainWidgetWithHandle vty child =
  runVtyAppWithHandle vty $ \dr0 inp -> do
    size <- holdDyn dr0 $ fforMaybe inp $ \case
      V.EvResize w h -> Just (w, h)
      _ -> Nothing
    let inp' = fforMaybe inp $ \case
          V.EvResize {} -> Nothing
          x -> Just x
    (shutdown, images) <- runFocusReader (pure True) $ runDisplayRegion (fmap (\(w, h) -> Region 0 0 w h) size) $
      runImageWriter $
        runNodeIdT $
          runInput inp' $ do
            tellImages . ffor (current size) $ \(w, h) -> [V.charFill V.defAttr ' ' w h]
            child
    return $ VtyResult
      { _vtyResult_picture = fmap (V.picForLayers . reverse) images
      , _vtyResult_shutdown = shutdown
      }

-- | The output of a vty widget
data VtyWidgetOut t = VtyWidgetOut
  { _vtyWidgetOut_shutdown :: Event t ()
  }

-- | Like 'mainWidgetWithHandle', but uses a default vty configuration
mainWidget
  :: (forall t m.
      ( MonadVtyApp t m
      , HasImageWriter t m
      , MonadNodeId m
      , HasDisplayRegion t m
      , HasFocusReader t m
      , HasInput t m
      ) => m (Event t ()))
  -> IO ()
mainWidget child = do
  vty <- getDefaultVty
  mainWidgetWithHandle vty child

-- * Input Events

-- | A class for things that can receive vty events as input
class HasInput t m | m -> t where
  input :: m (Event t VtyEvent)
  default input :: (f m' ~ m, Monad m', MonadTrans f, HasInput t m') => m (Event t VtyEvent)
  input = lift input
  -- | User input events that the widget's parent chooses to share. These will generally
  -- be filtered for relevance.
  localInput :: (Event t VtyEvent -> Event t VtyEvent) -> m a -> m a
  default localInput :: (f m' ~ m, Monad m', MFunctor f, HasInput t m') => (Event t VtyEvent -> Event t VtyEvent) -> m a -> m a
  localInput f = hoist (localInput f)

instance (Reflex t, Monad m) => HasInput t (Input t m) where
  input = Input ask
  localInput f (Input m) = Input $ local f m

-- | A widget that can receive input events. See 'Graphics.Vty.Event'
newtype Input t m a = Input
  { unInput :: ReaderT (Event t VtyEvent) m a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadSample t
    , MonadHold t
    , MonadFix
    , MonadIO
    , MonadRef
    )

instance (Adjustable t m, MonadHold t m, Reflex t) => Adjustable t (Input t m) where
  runWithReplace a0 a' = Input $ runWithReplace (unInput a0) $ fmap unInput a'
  traverseIntMapWithKeyWithAdjust f dm0 dm' = Input $
    traverseIntMapWithKeyWithAdjust (\k v -> unInput (f k v)) dm0 dm'
  traverseDMapWithKeyWithAdjust f dm0 dm' = Input $ do
    traverseDMapWithKeyWithAdjust (\k v -> unInput (f k v)) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = Input $ do
    traverseDMapWithKeyWithAdjustWithMove (\k v -> unInput (f k v)) dm0 dm'

deriving instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (Input t m)
deriving instance NotReady t m => NotReady t (Input t m)
deriving instance PerformEvent t m => PerformEvent t (Input t m)
deriving instance PostBuild t m => PostBuild t (Input t m)
deriving instance TriggerEvent t m => TriggerEvent t (Input t m)
instance HasImageWriter t m => HasImageWriter t (Input t m)
instance HasDisplayRegion t m => HasDisplayRegion t (Input t m)
instance HasFocusReader t m => HasFocusReader t (Input t m)

instance MonadTrans (Input t) where
  lift f = Input $ lift f

instance MFunctor (Input t) where
  hoist f = Input . hoist f . unInput

instance MonadNodeId m => MonadNodeId (Input t m)

-- | Runs an 'Input' with a given context
runInput
  :: Reflex t
  => Event t VtyEvent
  -> Input t m a
  -> m a
runInput e w = runReaderT (unInput w) e

-- ** Filtering input

-- | Type synonym for a key and modifier combination
type KeyCombo = (V.Key, [V.Modifier])

-- | Emits an event that fires on a particular key press (without modifiers)
key :: (Monad m, Reflex t, HasInput t m) => V.Key -> m (Event t KeyCombo)
key = keyCombos . Set.singleton . (,[])

-- | Emits an event that fires on particular key presses (without modifiers)
keys :: (Monad m, Reflex t, HasInput t m) => [V.Key] -> m (Event t KeyCombo)
keys = keyCombos . Set.fromList . fmap (,[])

-- | Emit an event that fires whenever the provided key combination occurs
keyCombo
  :: (Reflex t, Monad m, HasInput t m)
  => KeyCombo
  -> m (Event t KeyCombo)
keyCombo = keyCombos . Set.singleton

-- | Emit an event that fires whenever any of the provided key combinations occur
keyCombos
  :: (Reflex t, Monad m, HasInput t m)
  => Set KeyCombo
  -> m (Event t KeyCombo)
keyCombos ks = do
  i <- input
  return $ fforMaybe i $ \case
    V.EvKey k m -> if Set.member (k, m) ks
      then Just (k, m)
      else Nothing
    _ -> Nothing

-- | Filter the keyboard input that a child widget may receive
filterKeys :: (Reflex t, HasInput t m) => (KeyCombo -> Bool) -> m a -> m a
filterKeys f x = localInput (ffilter (\case
  V.EvKey k mods -> f (k, mods)
  _ -> True)) x

-- | Filter mouse input events based on whether they target a particular region
-- and translate them to the internal coordinate system of that region.
--
-- NB: Non-mouse events are passed through unfiltered and unchanged
mouseInRegion :: Region -> VtyEvent -> Maybe VtyEvent
mouseInRegion (Region l t w h) e = case e of
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

-- | Filter mouse input outside the current display region and
-- all input if the region is not focused
inputInFocusedRegion
  :: (HasDisplayRegion t m, HasFocusReader t m, HasInput t m)
  => m (Event t VtyEvent)
inputInFocusedRegion = do
  inp <- input
  reg <- current <$> askRegion
  foc <- current <$> focus
  pure $ fmapMaybe id $ attachWith filterInput ((,) <$> reg <*> foc) inp
  where
    filterInput (r, f) = \case
      V.EvKey {} | not f -> Nothing
      x -> mouseInRegion r x

-- * Getting and setting the display region

-- | A chunk of the display area
data Region = Region
  { _region_left :: Int
  , _region_top :: Int
  , _region_width :: Int
  , _region_height :: Int
  }
  deriving (Show, Read, Eq, Ord)

-- | A region that occupies no space.
nilRegion :: Region
nilRegion = Region 0 0 0 0

-- | The width and height of a 'Region'
regionSize :: Region -> (Int, Int)
regionSize (Region _ _ w h) = (w, h)

-- | Produces an 'Image' that fills a region with space characters
regionBlankImage :: Region -> Image
regionBlankImage r@(Region _ _ width height) =
  withinImage r $ V.charFill V.defAttr ' ' width height

-- | A class for things that know their own display size dimensions
class (Reflex t, Monad m) => HasDisplayRegion t m | m -> t where
  -- | Retrieve the display region
  askRegion :: m (Dynamic t Region)
  default askRegion :: (f m' ~ m, MonadTrans f, HasDisplayRegion t m') => m (Dynamic t Region)
  askRegion = lift askRegion
  -- | Run an action in a local region, by applying a transformation to the region
  localRegion :: (Dynamic t Region -> Dynamic t Region) -> m a -> m a
  default localRegion :: (f m' ~ m, Monad m', MFunctor f, HasDisplayRegion t m') => (Dynamic t Region -> Dynamic t Region) -> m a -> m a
  localRegion f = hoist (localRegion f)

-- | Retrieve the display width
displayWidth :: HasDisplayRegion t m => m (Dynamic t Int)
displayWidth = fmap _region_width <$> askRegion

-- | Retrieve the display height
displayHeight :: HasDisplayRegion t m => m (Dynamic t Int)
displayHeight = fmap _region_height <$> askRegion

instance HasDisplayRegion t m => HasDisplayRegion t (ReaderT x m)
instance HasDisplayRegion t m => HasDisplayRegion t (BehaviorWriterT t x m)
instance HasDisplayRegion t m => HasDisplayRegion t (DynamicWriterT t x m)
instance HasDisplayRegion t m => HasDisplayRegion t (EventWriterT t x m)
instance HasDisplayRegion t m => HasDisplayRegion t (NodeIdT m)

-- | A widget that has access to a particular region of the vty display
newtype DisplayRegion t m a = DisplayRegion
  { unDisplayRegion :: ReaderT (Dynamic t Region) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadHold t
    , MonadIO
    , MonadRef
    , MonadSample t
    )

instance (Monad m, Reflex t) => HasDisplayRegion t (DisplayRegion t m) where
  askRegion = DisplayRegion ask
  localRegion f = DisplayRegion . local f . unDisplayRegion

deriving instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (DisplayRegion t m)
deriving instance NotReady t m => NotReady t (DisplayRegion t m)
deriving instance PerformEvent t m => PerformEvent t (DisplayRegion t m)
deriving instance PostBuild t m => PostBuild t (DisplayRegion t m)
deriving instance TriggerEvent t m => TriggerEvent t (DisplayRegion t m)
instance HasImageWriter t m => HasImageWriter t (DisplayRegion t m)
instance HasFocusReader t m => HasFocusReader t (DisplayRegion t m)

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (DisplayRegion t m) where
  runWithReplace (DisplayRegion a) e = DisplayRegion $ runWithReplace a $ fmap unDisplayRegion e
  traverseIntMapWithKeyWithAdjust f m e = DisplayRegion $ traverseIntMapWithKeyWithAdjust (\k v -> unDisplayRegion $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = DisplayRegion $ traverseDMapWithKeyWithAdjust (\k v -> unDisplayRegion $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = DisplayRegion $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unDisplayRegion $ f k v) m e

instance MonadTrans (DisplayRegion t) where
  lift = DisplayRegion . lift

instance MFunctor (DisplayRegion t) where
  hoist f = DisplayRegion . hoist f . unDisplayRegion

instance MonadNodeId m => MonadNodeId (DisplayRegion t m)

-- | Run a 'DisplayRegion' action with a given 'Region'
runDisplayRegion
  :: (Reflex t, Monad m)
  => Dynamic t Region
  -> DisplayRegion t m a
  -> m a
runDisplayRegion r = flip runReaderT r . unDisplayRegion

-- * Getting focus state

-- | A class for things that can dynamically gain and lose focus
class (Reflex t, Monad m) => HasFocusReader t m | m -> t where
  focus :: m (Dynamic t Bool)
  default focus :: (f m' ~ m, Monad m', MonadTrans f, HasFocusReader t m') => m (Dynamic t Bool)
  focus = lift focus
  localFocus :: (Dynamic t Bool -> Dynamic t Bool) -> m a -> m a
  default localFocus :: (f m' ~ m, Monad m', MFunctor f, HasFocusReader t m') => (Dynamic t Bool -> Dynamic t Bool) -> m a -> m a
  localFocus f = hoist (localFocus f)

instance HasFocusReader t m => HasFocusReader t (ReaderT x m)
instance HasFocusReader t m => HasFocusReader t (BehaviorWriterT t x m)
instance HasFocusReader t m => HasFocusReader t (DynamicWriterT t x m)
instance HasFocusReader t m => HasFocusReader t (EventWriterT t x m)
instance HasFocusReader t m => HasFocusReader t (NodeIdT m)

-- | A widget that has access to information about whether it is focused
newtype FocusReader t m a = FocusReader
  { unFocusReader :: ReaderT (Dynamic t Bool) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadHold t
    , MonadIO
    , MonadRef
    , MonadSample t
    )

instance (Monad m, Reflex t) => HasFocusReader t (FocusReader t m) where
  focus = FocusReader ask
  localFocus f = FocusReader . local f . unFocusReader

deriving instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (FocusReader t m)
deriving instance NotReady t m => NotReady t (FocusReader t m)
deriving instance PerformEvent t m => PerformEvent t (FocusReader t m)
deriving instance PostBuild t m => PostBuild t (FocusReader t m)
deriving instance TriggerEvent t m => TriggerEvent t (FocusReader t m)
instance HasImageWriter t m => HasImageWriter t (FocusReader t m)

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (FocusReader t m) where
  runWithReplace (FocusReader a) e = FocusReader $ runWithReplace a $ fmap unFocusReader e
  traverseIntMapWithKeyWithAdjust f m e = FocusReader $ traverseIntMapWithKeyWithAdjust (\k v -> unFocusReader $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = FocusReader $ traverseDMapWithKeyWithAdjust (\k v -> unFocusReader $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = FocusReader $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unFocusReader $ f k v) m e

instance MonadTrans (FocusReader t) where
  lift = FocusReader . lift

instance MFunctor (FocusReader t) where
  hoist f = FocusReader . hoist f . unFocusReader

instance MonadNodeId m => MonadNodeId (FocusReader t m)

-- | Run a 'FocusReader' action with the given focus value
runFocusReader
  :: (Reflex t, Monad m)
  => Dynamic t Bool
  -> FocusReader t m a
  -> m a
runFocusReader b = flip runReaderT b . unFocusReader

-- * "Image" output

-- | A class for widgets that can produce images to draw to the display
class (Reflex t, Monad m) => HasImageWriter t m | m -> t where
  -- | Send images upstream for rendering
  tellImages :: Behavior t [Image] -> m ()
  default tellImages :: (f m' ~ m, Monad m', MonadTrans f, HasImageWriter t m') => Behavior t [Image] -> m ()
  tellImages = lift . tellImages
  -- | Apply a transformation to the images produced by the child actions
  mapImages :: (Behavior t [Image] -> Behavior t [Image]) -> m a -> m a
  default mapImages :: (f m' ~ m, Monad m', MFunctor f, HasImageWriter t m') => (Behavior t [Image] -> Behavior t [Image]) -> m a -> m a
  mapImages f = hoist (mapImages f)

-- | A widget that can produce images to draw onto the display
newtype ImageWriter t m a = ImageWriter
  { unImageWriter :: BehaviorWriterT t [Image] m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadHold t
    , MonadIO
    , MonadRef
    , MonadReflexCreateTrigger t
    , MonadSample t
    , NotReady t
    , PerformEvent t
    , PostBuild t
    , TriggerEvent t 
    )

instance MonadTrans (ImageWriter t) where
  lift = ImageWriter . lift

instance MFunctor (ImageWriter t) where
  hoist f = ImageWriter . (hoist f) . unImageWriter

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (ImageWriter t m) where
  runWithReplace (ImageWriter a) e = ImageWriter $ runWithReplace a $ fmap unImageWriter e
  traverseIntMapWithKeyWithAdjust f m e = ImageWriter $ traverseIntMapWithKeyWithAdjust (\k v -> unImageWriter $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = ImageWriter $ traverseDMapWithKeyWithAdjust (\k v -> unImageWriter $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = ImageWriter $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unImageWriter $ f k v) m e

instance HasImageWriter t m => HasImageWriter t (ReaderT x m)
instance HasImageWriter t m => HasImageWriter t (BehaviorWriterT t x m)
instance HasImageWriter t m => HasImageWriter t (DynamicWriterT t x m)
instance HasImageWriter t m => HasImageWriter t (EventWriterT t x m)
instance HasImageWriter t m => HasImageWriter t (NodeIdT m)

instance (Monad m, Reflex t) => HasImageWriter t (ImageWriter t m) where
  tellImages = ImageWriter . tellBehavior
  mapImages f (ImageWriter x) = ImageWriter $ do
    (a, images) <- lift $ runBehaviorWriterT x
    tellBehavior $ f images
    pure a

instance HasDisplayRegion t m => HasDisplayRegion t (ImageWriter t m)
instance HasFocusReader t m => HasFocusReader t (ImageWriter t m)

-- | Run a widget that can produce images
runImageWriter
  :: (Reflex t, Monad m)
  => ImageWriter t m a
  -> m (a, Behavior t [Image])
runImageWriter = runBehaviorWriterT . unImageWriter

-- ** Manipulating images

-- | Translates and crops an 'Image' so that it is contained by
-- the given 'Region'.
withinImage
  :: Region
  -> Image
  -> Image
withinImage (Region left top width height)
  | width < 0 || height < 0 = withinImage (Region left top 0 0)
  | otherwise = V.translate left top . V.crop width height

-- | Crop a behavior of images to a behavior of regions. See 'withinImage'.
imagesInRegion
  :: Reflex t
  => Behavior t Region
  -> Behavior t [Image]
  -> Behavior t [Image]
imagesInRegion reg = liftA2 (\r is -> map (withinImage r) is) reg

-- * Running sub-widgets

-- | Low-level widget combinator that runs a child widget within
-- a given region and context. This widget filters and modifies the input
-- that the child widget receives such that:
-- * unfocused widgets receive no key events
-- * mouse inputs outside the region are ignored
-- * mouse inputs inside the region have their coordinates translated such
--   that (0,0) is the top-left corner of the region
pane
  :: (Reflex t, Monad m, HasInput t m, HasImageWriter t m, HasDisplayRegion t m, HasFocusReader t m)
  => Dynamic t Region
  -> Dynamic t Bool -- ^ Whether the widget should be focused when the parent is.
  -> m a
  -> m a
pane dr foc child = localRegion (const dr) $
  mapImages (imagesInRegion $ current dr) $
    localFocus (const foc) $
      inputInFocusedRegion >>= \e -> localInput (const e) child

-- * Misc

-- | A widget that draws nothing
blank :: Monad m => m ()
blank = return ()
