{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Reflex.Vty.Widget
-- Description: Basic set of widgets and building blocks for reflex-vty applications
module Reflex.Vty.Widget where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader (ReaderT (..), ask, local, runReaderT)
import Control.Monad.Ref
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.State.Strict
import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.Vty (Image)
import qualified Graphics.Vty as V
import Reflex
import Reflex.Class ()
import Reflex.Host.Class (MonadReflexCreateTrigger)

import Control.Monad.NodeId
import Reflex.Vty.ColorProfile
import Reflex.Vty.Host
import Reflex.Vty.Theme (Theme (..), defTheme, themeToAttr)

-- * Running a vty application

-- | Sets up the top-level context for a vty widget and runs it with that context
mainWidgetWithHandle
  :: VtyAppConfig
  -> V.Vty
  -> ( forall t m
         . ( MonadVtyApp t m
           , HasImageWriter t m
           , MonadNodeId m
           , HasDisplayRegion t m
           , HasFocusReader t m
           , HasInput t m
           , HasTheme t m
           , HasColorProfile t m
           )
        => m (Event t ())
      )
  -> IO ()
mainWidgetWithHandle cfg vty child =
  runVtyAppWithHandle cfg vty $ \dr0 inp -> do
    let profile = colorProfileFromVty vty
    size <- holdDyn dr0 $ fforMaybe inp $ \case
      V.EvResize w h -> Just (w, h)
      _ -> Nothing
    let inp' = fforMaybe inp $ \case
          V.EvResize {} -> Nothing
          x -> Just x
    (shutdown, images) <- runThemeReader (constant defTheme) $
      runColorProfileReader (constant profile) $
        runFocusReader (pure True) $
          runDisplayRegion (fmap (\(w, h) -> Region 0 0 w h) size) $
            runImageWriter $
              runNodeIdT $
                runInput inp' $ do
                  tellImages . ffor (current size) $ \(w, h) -> [V.charFill V.defAttr ' ' w h]
                  child
    return $
      VtyResult
        { _vtyResult_picture = fmap (V.picForLayers . reverse) images
        , _vtyResult_shutdown = shutdown
        }

-- | The output of a vty widget
data VtyWidgetOut t = VtyWidgetOut
  { _vtyWidgetOut_shutdown :: Event t ()
  }

-- | Like 'mainWidgetWithHandle', but uses a default vty configuration
mainWidget
  :: VtyAppConfig
  -> ( forall t m
         . ( MonadVtyApp t m
           , HasImageWriter t m
           , MonadNodeId m
           , HasDisplayRegion t m
           , HasFocusReader t m
           , HasTheme t m
           , HasColorProfile t m
           , HasInput t m
           )
        => m (Event t ())
      )
  -> IO ()
mainWidget cfg child = do
  vty <- getDefaultVty
  mainWidgetWithHandle cfg vty child

-- * Input Events

-- | A class for things that can receive vty events as input
class HasInput t m | m -> t where
  input :: m (Event t VtyEvent)
  default input :: (f m' ~ m, Monad m', MonadTrans f, HasInput t m') => m (Event t VtyEvent)
  input = lift input

  -- | User input events that the widget's parent chooses to share. These will generally
  --   be filtered for relevance.
  localInput :: (Event t VtyEvent -> Event t VtyEvent) -> m a -> m a
  default localInput :: (f m' ~ m, Monad m', MFunctor f, HasInput t m') => (Event t VtyEvent -> Event t VtyEvent) -> m a -> m a
  localInput f = hoist (localInput f)

instance (Reflex t, Monad m) => HasInput t (Input t m) where
  input = Input ask
  localInput f (Input m) = Input $ local f m

-- | A widget that can receive input events. See 'Graphics.Vty.Event'
newtype Input t m a = Input
  { unInput :: ReaderT (Event t VtyEvent) m a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadFix
    , MonadHold t
    , MonadIO
    , MonadMask
    , MonadRef
    , MonadSample t
    , MonadThrow
    )

instance (Adjustable t m, MonadHold t m, Reflex t) => Adjustable t (Input t m) where
  runWithReplace a0 a' = Input $ runWithReplace (unInput a0) $ fmap unInput a'
  traverseIntMapWithKeyWithAdjust f dm0 dm' =
    Input $
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
instance HasImageWriter t m => HasImageWriter t (Input t m) where
  captureImages x = do
    a <- input
    lift $ captureImages $ runInput a x
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
    V.EvKey k m ->
      if Set.member (k, m) ks
        then Just (k, m)
        else Nothing
    _ -> Nothing

-- | Filter the keyboard input that a child widget may receive
filterKeys :: (Reflex t, HasInput t m) => (KeyCombo -> Bool) -> m a -> m a
filterKeys f x =
  localInput
    ( ffilter
        ( \case
            V.EvKey k mods -> f (k, mods)
            _ -> True
        )
    )
    x

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
      | or
          [ x < l
          , y < t
          , x >= l + w
          , y >= t + h
          ] =
          Nothing
      | otherwise =
          Just (con (x - l) (y - t))

-- |
-- * 'Tracking' state means actively tracking the current stream of mouse events
-- * 'NotTracking' state means not tracking the current stream of mouse events
-- * 'WaitingForInput' means state will be set on next 'EvMouseDown' event
data MouseTrackingState = Tracking V.Button | NotTracking | WaitingForInput deriving (Eq, Show)

-- | Filter mouse input outside the current display region
-- keyboard input is reported only if the region is focused
-- scroll wheel input is reported only if the region is focused
-- mouse input is reported if the mouse is in the region
-- EXCEPT mouse drag sequences that start OFF the region are NOT reported
-- AND mouse drag sequences that start ON the region and drag off ARE reported
inputInFocusedRegion
  :: forall t m
   . (MonadFix m, MonadHold t m, HasDisplayRegion t m, HasFocusReader t m, HasInput t m)
  => m (Event t VtyEvent)
inputInFocusedRegion = do
  inp <- input
  regBeh <- current <$> askRegion
  foc <- current <$> focus
  let trackMouse
        :: VtyEvent
        -> (MouseTrackingState, Maybe VtyEvent)
        -> PushM t (Maybe (MouseTrackingState, Maybe VtyEvent))
      trackMouse e (tracking, _) = do
        -- sampling (as oppose to using attachPromptlyDyn) is necessary here as the focus may change from the event produced here
        focused <- sample foc
        -- strictly speaking the same could also happen here too
        reg@(Region l t _ _) <- sample regBeh
        return $ case e of
          -- filter keyboard input if region is not focused
          V.EvKey _ _ | not focused -> Nothing
          -- filter scroll wheel input based on mouse position
          V.EvMouseDown x y btn m | btn == V.BScrollUp || btn == V.BScrollDown -> case tracking of
            trck@(Tracking _) -> Just (trck, Nothing)
            _ -> Just (WaitingForInput, if withinRegion reg x y then Just (V.EvMouseDown (x - l) (y - t) btn m) else Nothing)
          -- only do tracking for l/m/r mouse buttons
          V.EvMouseDown x y btn m ->
            if tracking == Tracking btn || (tracking == WaitingForInput && withinRegion reg x y)
              then Just (Tracking btn, Just $ V.EvMouseDown (x - l) (y - t) btn m)
              else Just (NotTracking, Nothing)
          V.EvMouseUp x y mbtn -> case mbtn of
            Nothing -> case tracking of
              Tracking _ -> Just (WaitingForInput, Just $ V.EvMouseUp (x - l) (y - t) mbtn)
              _ -> Just (WaitingForInput, Nothing)
            Just btn ->
              if tracking == Tracking btn
                -- NOTE we only report EvMouseUp for the button we are tracking
                -- vty has mouse buttons override others (seems to be based on ordering of Button) when multiple are pressed.
                -- so it IS possible for child widget to miss out on a 'EvMouseUp' event with this current implementation
                then Just (WaitingForInput, Just $ V.EvMouseUp (x - l) (y - t) mbtn)
                else Just (WaitingForInput, Nothing)
          _ -> Just (tracking, Just e)
  dynInputEvTracking <- foldDynMaybeM trackMouse (WaitingForInput, Nothing) $ inp
  return (fmapMaybe snd $ updated dynInputEvTracking)

-- * Getting and setting the display region

-- | A chunk of the display area
data Region = Region
  { _region_left :: Int
  , _region_top :: Int
  , _region_width :: Int
  , _region_height :: Int
  }
  deriving (Eq, Ord, Read, Show)

-- | A region that occupies no space.
nilRegion :: Region
nilRegion = Region 0 0 0 0

-- | The width and height of a 'Region'
regionSize :: Region -> (Int, Int)
regionSize (Region _ _ w h) = (w, h)

-- | Check whether the x,y coordinates are within the specified region
withinRegion
  :: Region
  -> Int
  -- ^ x-coordinate
  -> Int
  -- ^ y-coordinate
  -> Bool
withinRegion (Region l t w h) x y =
  not . or $
    [ x < l
    , y < t
    , x >= l + w
    , y >= t + h
    ]

-- | Produces an 'Image' that fills a region with space characters
regionBlankImage :: V.Attr -> Region -> Image
regionBlankImage attr r@(Region _ _ width height) =
  withinImage r $ V.charFill attr ' ' width height

-- | A class for things that know their own display size dimensions
class (Reflex t, Monad m) => HasDisplayRegion t m | m -> t where
  -- | Retrieve the display region. This is the "layout region" — the
  -- space the layout system uses to distribute among children. Inside
  -- 'scrollable', this may be larger than the actual viewport.
  askRegion :: m (Dynamic t Region)
  default askRegion :: (f m' ~ m, MonadTrans f, HasDisplayRegion t m') => m (Dynamic t Region)
  askRegion = lift askRegion

  -- | Run an action in a local region, by applying a transformation to
  -- the region. Affects both layout and viewport.
  localRegion :: (Dynamic t Region -> Dynamic t Region) -> m a -> m a
  default localRegion :: (f m' ~ m, Monad m', MFunctor f, HasDisplayRegion t m') => (Dynamic t Region -> Dynamic t Region) -> m a -> m a
  localRegion f = hoist (localRegion f)

  -- | The actual on-screen viewport region. May differ from 'askRegion'
  -- inside containers that inflate the layout region (e.g. 'scrollable').
  -- Use this for rendering decisions: how many rows\/cols to fill, how
  -- tall to draw a box, etc.
  askViewport :: m (Dynamic t Region)
  default askViewport :: (f m' ~ m, MonadTrans f, HasDisplayRegion t m') => m (Dynamic t Region)
  askViewport = lift askViewport

  -- | Run an action with a modified layout region only, leaving the
  -- viewport unchanged. Used by 'scrollable' to give children more
  -- layout height than is visible, while widgets still know the real
  -- viewport size for rendering.
  localLayoutRegion :: (Dynamic t Region -> Dynamic t Region) -> m a -> m a
  default localLayoutRegion :: (f m' ~ m, Monad m', MFunctor f, HasDisplayRegion t m') => (Dynamic t Region -> Dynamic t Region) -> m a -> m a
  localLayoutRegion f = hoist (localLayoutRegion f)

-- | Retrieve the display width
displayWidth :: HasDisplayRegion t m => m (Dynamic t Int)
displayWidth = fmap _region_width <$> askRegion

-- | Retrieve the display height
displayHeight :: HasDisplayRegion t m => m (Dynamic t Int)
displayHeight = fmap _region_height <$> askRegion

-- | Retrieve the viewport width (actual on-screen width)
viewportWidth :: HasDisplayRegion t m => m (Dynamic t Int)
viewportWidth = fmap _region_width <$> askViewport

-- | Retrieve the viewport height (actual on-screen height)
viewportHeight :: HasDisplayRegion t m => m (Dynamic t Int)
viewportHeight = fmap _region_height <$> askViewport

instance HasDisplayRegion t m => HasDisplayRegion t (ReaderT x m)
instance HasDisplayRegion t m => HasDisplayRegion t (BehaviorWriterT t x m)
instance HasDisplayRegion t m => HasDisplayRegion t (DynamicWriterT t x m)
instance HasDisplayRegion t m => HasDisplayRegion t (EventWriterT t x m)
instance HasDisplayRegion t m => HasDisplayRegion t (NodeIdT m)

-- | A widget that has access to a particular region of the vty display
newtype DisplayRegion t m a = DisplayRegion
  {unDisplayRegion :: ReaderT (Dynamic t (Region, Region)) m a}
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadFix
    , MonadHold t
    , MonadIO
    , MonadMask
    , MonadRef
    , MonadSample t
    , MonadThrow
    )

instance (Monad m, Reflex t) => HasDisplayRegion t (DisplayRegion t m) where
  askRegion = DisplayRegion $ fmap fst <$> ask
  localRegion f = DisplayRegion . local (\dr -> (,) <$> f (fst <$> dr) <*> f (snd <$> dr)) . unDisplayRegion
  askViewport = DisplayRegion $ fmap snd <$> ask
  localLayoutRegion f = DisplayRegion . local (\dr -> (,) <$> f (fst <$> dr) <*> (snd <$> dr)) . unDisplayRegion

deriving instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (DisplayRegion t m)
deriving instance NotReady t m => NotReady t (DisplayRegion t m)
deriving instance PerformEvent t m => PerformEvent t (DisplayRegion t m)
deriving instance PostBuild t m => PostBuild t (DisplayRegion t m)
deriving instance TriggerEvent t m => TriggerEvent t (DisplayRegion t m)
instance HasImageWriter t m => HasImageWriter t (DisplayRegion t m) where
  captureImages x = do
    reg <- askRegion
    vp <- askViewport
    lift $ captureImages $ runDisplayRegionBoth reg vp x
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

-- | Run a 'DisplayRegion' action with a given 'Region'. Sets both the
-- layout region and viewport to the same value.
runDisplayRegion
  :: (Reflex t, Monad m)
  => Dynamic t Region
  -> DisplayRegion t m a
  -> m a
runDisplayRegion r = runDisplayRegionBoth r r

-- | Run a 'DisplayRegion' action with separate layout and viewport regions.
runDisplayRegionBoth
  :: (Reflex t, Monad m)
  => Dynamic t Region
  -- ^ Layout region (what 'askRegion' returns)
  -> Dynamic t Region
  -- ^ Viewport region (what 'askViewport' returns)
  -> DisplayRegion t m a
  -> m a
runDisplayRegionBoth layout viewport =
  flip runReaderT ((,) <$> layout <*> viewport) . unDisplayRegion

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
  {unFocusReader :: ReaderT (Dynamic t Bool) m a}
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadFix
    , MonadHold t
    , MonadIO
    , MonadMask
    , MonadRef
    , MonadSample t
    , MonadThrow
    )

instance (Monad m, Reflex t) => HasFocusReader t (FocusReader t m) where
  focus = FocusReader ask
  localFocus f = FocusReader . local f . unFocusReader

deriving instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (FocusReader t m)
deriving instance NotReady t m => NotReady t (FocusReader t m)
deriving instance PerformEvent t m => PerformEvent t (FocusReader t m)
deriving instance PostBuild t m => PostBuild t (FocusReader t m)
deriving instance TriggerEvent t m => TriggerEvent t (FocusReader t m)
instance HasImageWriter t m => HasImageWriter t (FocusReader t m) where
  captureImages x = do
    a <- focus
    lift $ captureImages $ runFocusReader a x

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
class (Reflex t, Monad m) => HasImageWriter (t :: Type) m | m -> t where
  -- | Send images upstream for rendering
  tellImages :: Behavior t [Image] -> m ()
  default tellImages :: (f m' ~ m, Monad m', MonadTrans f, HasImageWriter t m') => Behavior t [Image] -> m ()
  tellImages = lift . tellImages

  -- | Apply a transformation to the images produced by the child actions
  mapImages :: (Behavior t [Image] -> Behavior t [Image]) -> m a -> m a
  default mapImages :: (f m' ~ m, Monad m', MFunctor f, HasImageWriter t m') => (Behavior t [Image] -> Behavior t [Image]) -> m a -> m a
  mapImages f = hoist (mapImages f)

  -- | Capture images, preventing them from being drawn
  captureImages :: m a -> m (a, Behavior t [Image])

-- | A widget that can produce images to draw onto the display
newtype ImageWriter t m a = ImageWriter
  {unImageWriter :: BehaviorWriterT t [Image] m a}
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadFix
    , MonadHold t
    , MonadIO
    , MonadMask
    , MonadRef
    , MonadReflexCreateTrigger t
    , MonadSample t
    , MonadThrow
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

instance HasImageWriter t m => HasImageWriter t (ReaderT x m) where
  captureImages x = do
    a <- ask
    lift $ captureImages $ runReaderT x a
instance HasImageWriter t m => HasImageWriter t (BehaviorWriterT t x m) where
  captureImages (BehaviorWriterT x) = BehaviorWriterT $ do
    s <- get
    ((result, s'), images) <- lift $ captureImages $ runStateT x s
    put s'
    return (result, images)
instance HasImageWriter t m => HasImageWriter t (DynamicWriterT t x m) where
  captureImages (DynamicWriterT x) = DynamicWriterT $ do
    s <- get
    ((result, s'), images) <- lift $ captureImages $ runStateT x s
    put s'
    return (result, images)

instance HasImageWriter t m => HasImageWriter t (EventWriterT t x m) where
  captureImages (EventWriterT x) = EventWriterT $ do
    s <- get
    ((result, s'), images) <- lift $ captureImages $ runStateT x s
    put s'
    return (result, images)

instance HasImageWriter t m => HasImageWriter t (NodeIdT m) where
  captureImages x = NodeIdT $ do
    ref <- ask
    lift $ captureImages $ flip runReaderT ref . unNodeIdT $ x

instance (Monad m, Reflex t) => HasImageWriter t (ImageWriter t m) where
  tellImages = ImageWriter . tellBehavior
  mapImages f (ImageWriter x) = ImageWriter $ do
    (a, images) <- lift $ runBehaviorWriterT x
    tellBehavior $ f images
    pure a
  captureImages (ImageWriter x) = ImageWriter $ do
    lift $ runBehaviorWriterT x

instance HasDisplayRegion t m => HasDisplayRegion t (ImageWriter t m)
instance HasFocusReader t m => HasFocusReader t (ImageWriter t m)

-- | Run a widget that can produce images
runImageWriter
  :: (Reflex t, Monad m)
  => ImageWriter t m a
  -> m (a, Behavior t [Image])
runImageWriter = runBehaviorWriterT . unImageWriter

-- * Theming

-- | A class for things that can be visually styled
class (Reflex t, Monad m) => HasTheme t m | m -> t where
  theme :: m (Behavior t Theme)
  default theme :: (f m' ~ m, Monad m', MonadTrans f, HasTheme t m') => m (Behavior t Theme)
  theme = lift theme

  -- | Convenience: the ambient 'V.Attr' from '_theme_default'. Most widgets
  --   only need this.
  themeAttr :: m (Behavior t V.Attr)
  default themeAttr :: (f m' ~ m, Monad m', MonadTrans f, HasTheme t m') => m (Behavior t V.Attr)
  themeAttr = lift themeAttr

  localTheme :: (Behavior t Theme -> Behavior t Theme) -> m a -> m a
  default localTheme :: (f m' ~ m, Monad m', MFunctor f, HasTheme t m') => (Behavior t Theme -> Behavior t Theme) -> m a -> m a
  localTheme f = hoist (localTheme f)

instance HasTheme t m => HasTheme t (ReaderT x m)
instance HasTheme t m => HasTheme t (BehaviorWriterT t x m)
instance HasTheme t m => HasTheme t (DynamicWriterT t x m)
instance HasTheme t m => HasTheme t (EventWriterT t x m)
instance HasTheme t m => HasTheme t (NodeIdT m)
instance HasTheme t m => HasTheme t (Input t m)
instance HasTheme t m => HasTheme t (ImageWriter t m)
instance HasTheme t m => HasTheme t (DisplayRegion t m)
instance HasTheme t m => HasTheme t (FocusReader t m)

-- | A widget that has access to theme information
newtype ThemeReader t m a = ThemeReader
  {unThemeReader :: ReaderT (Behavior t Theme) m a}
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadFix
    , MonadHold t
    , MonadIO
    , MonadMask
    , MonadRef
    , MonadSample t
    , MonadThrow
    )

instance (Monad m, Reflex t) => HasTheme t (ThemeReader t m) where
  theme = ThemeReader ask
  themeAttr = Reflex.Vty.Theme.themeToAttr <$> ThemeReader ask
  localTheme f = ThemeReader . local f . unThemeReader

deriving instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ThemeReader t m)
deriving instance NotReady t m => NotReady t (ThemeReader t m)
deriving instance PerformEvent t m => PerformEvent t (ThemeReader t m)
deriving instance PostBuild t m => PostBuild t (ThemeReader t m)
deriving instance TriggerEvent t m => TriggerEvent t (ThemeReader t m)
instance HasImageWriter t m => HasImageWriter t (ThemeReader t m) where
  captureImages x = ThemeReader $ do
    a <- ask
    lift $ captureImages $ flip runReaderT a $ unThemeReader x

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (ThemeReader t m) where
  runWithReplace (ThemeReader a) e = ThemeReader $ runWithReplace a $ fmap unThemeReader e
  traverseIntMapWithKeyWithAdjust f m e = ThemeReader $ traverseIntMapWithKeyWithAdjust (\k v -> unThemeReader $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = ThemeReader $ traverseDMapWithKeyWithAdjust (\k v -> unThemeReader $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = ThemeReader $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unThemeReader $ f k v) m e

instance MonadTrans (ThemeReader t) where
  lift = ThemeReader . lift

instance MFunctor (ThemeReader t) where
  hoist f = ThemeReader . hoist f . unThemeReader

instance MonadNodeId m => MonadNodeId (ThemeReader t m)

-- | Run a 'ThemeReader' action with the given theme
runThemeReader
  :: (Reflex t, Monad m)
  => Behavior t Theme
  -> ThemeReader t m a
  -> m a
runThemeReader b = flip runReaderT b . unThemeReader

-- * Color profile

-- | A class for widgets that need to know the terminal's color capability.
-- Widgets build with true-color 'V.Attr's and the host downsamples via
-- 'Reflex.Vty.ColorProfile.applyProfile' at the 'V.Picture' boundary, so
-- most widgets never need to call 'colorProfile' directly: it is useful
-- when a widget wants to make a structural decision based on capability
-- (e.g. choosing a different glyph for an 8-color terminal).
class (Reflex t, Monad m) => HasColorProfile t m | m -> t where
  colorProfile :: m (Behavior t ColorProfile)
  default colorProfile :: (f m' ~ m, Monad m', MonadTrans f, HasColorProfile t m') => m (Behavior t ColorProfile)
  colorProfile = lift colorProfile
  localColorProfile :: (Behavior t ColorProfile -> Behavior t ColorProfile) -> m a -> m a
  default localColorProfile :: (f m' ~ m, Monad m', MFunctor f, HasColorProfile t m') => (Behavior t ColorProfile -> Behavior t ColorProfile) -> m a -> m a
  localColorProfile f = hoist (localColorProfile f)

instance HasColorProfile t m => HasColorProfile t (ReaderT x m)
instance HasColorProfile t m => HasColorProfile t (BehaviorWriterT t x m)
instance HasColorProfile t m => HasColorProfile t (DynamicWriterT t x m)
instance HasColorProfile t m => HasColorProfile t (EventWriterT t x m)
instance HasColorProfile t m => HasColorProfile t (NodeIdT m)
instance HasColorProfile t m => HasColorProfile t (Input t m)
instance HasColorProfile t m => HasColorProfile t (ImageWriter t m)
instance HasColorProfile t m => HasColorProfile t (DisplayRegion t m)
instance HasColorProfile t m => HasColorProfile t (FocusReader t m)
instance HasColorProfile t m => HasColorProfile t (ThemeReader t m)

-- | A widget that has access to the terminal's 'ColorProfile'.
newtype ColorProfileReader t m a = ColorProfileReader
  {unColorProfileReader :: ReaderT (Behavior t ColorProfile) m a}
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadFix
    , MonadHold t
    , MonadIO
    , MonadMask
    , MonadRef
    , MonadSample t
    , MonadThrow
    )

instance (Monad m, Reflex t) => HasColorProfile t (ColorProfileReader t m) where
  colorProfile = ColorProfileReader ask
  localColorProfile f = ColorProfileReader . local f . unColorProfileReader

deriving instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ColorProfileReader t m)
deriving instance NotReady t m => NotReady t (ColorProfileReader t m)
deriving instance PerformEvent t m => PerformEvent t (ColorProfileReader t m)
deriving instance PostBuild t m => PostBuild t (ColorProfileReader t m)
deriving instance TriggerEvent t m => TriggerEvent t (ColorProfileReader t m)
instance HasImageWriter t m => HasImageWriter t (ColorProfileReader t m) where
  captureImages x = ColorProfileReader $ do
    a <- ask
    lift $ captureImages $ flip runReaderT a $ unColorProfileReader x
instance HasTheme t m => HasTheme t (ColorProfileReader t m)

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (ColorProfileReader t m) where
  runWithReplace (ColorProfileReader a) e = ColorProfileReader $ runWithReplace a $ fmap unColorProfileReader e
  traverseIntMapWithKeyWithAdjust f m e = ColorProfileReader $ traverseIntMapWithKeyWithAdjust (\k v -> unColorProfileReader $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = ColorProfileReader $ traverseDMapWithKeyWithAdjust (\k v -> unColorProfileReader $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = ColorProfileReader $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unColorProfileReader $ f k v) m e

instance MonadTrans (ColorProfileReader t) where
  lift = ColorProfileReader . lift

instance MFunctor (ColorProfileReader t) where
  hoist f = ColorProfileReader . hoist f . unColorProfileReader

instance MonadNodeId m => MonadNodeId (ColorProfileReader t m)

-- | Run a 'ColorProfileReader' action with the given profile.
runColorProfileReader
  :: (Reflex t, Monad m)
  => Behavior t ColorProfile
  -> ColorProfileReader t m a
  -> m a
runColorProfileReader b = flip runReaderT b . unColorProfileReader

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
-- * mouse inputs inside the region have their coordinates translated such
--   that (0,0) is the top-left corner of the region
-- * mouse drag sequences that start OFF the region are ignored
-- * mouse drag sequences that start ON the region and drag off are NOT ignored
pane
  :: (MonadFix m, MonadHold t m, HasInput t m, HasImageWriter t m, HasDisplayRegion t m, HasFocusReader t m)
  => Dynamic t Region
  -> Dynamic t Bool
  -- ^ Whether the widget should be focused when the parent is.
  -> m a
  -> m a
pane dr foc child =
  localRegion (const dr) $
    mapImages (imagesInRegion $ current dr) $
      localFocus (const foc) $
        inputInFocusedRegion >>= \e -> localInput (const e) child

-- * Misc

-- | A widget that draws nothing
blank :: Monad m => m ()
blank = return ()
