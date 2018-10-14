{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -Wall #-}

module Reflex.Vty
  ( VtyApp
  , VtyResult(..)
  , VtyEvent
  , runVtyApp
  , runVtyAppWith
  , HasDisplaySize(..)
  , HasFocus(..)
  , HasVtyInput(..)
  , Region(..)
  , regionToDisplayRegion
  , region
  , box
  , runVtyWidget
  , VtyWidgetCtx(..)
  , VtyWidget(..)
  , VtyWidgetOut(..)
  , pane
  , modifyImages
  , mainVtyWidget
  , filterInput
  , tellImages
  , tellShutdown
  , wrapString
  ) where

import Control.Applicative
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad (forM, forM_, forever)
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef, Ref, readRef)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (IORef)
import Data.IORef (readIORef)
import Data.Maybe (catMaybes)

import Reflex
import Reflex.Host.Class
import Reflex.NotReady.Class
import Reflex.Spider.Orphans ()
import qualified Graphics.Vty as V
import Graphics.Vty (DisplayRegion, Image, Attr)

type VtyEvent = V.Event

-- | The output of a 'VtyApp'.
data VtyResult t = VtyResult
  { _vtyResult_picture :: Behavior t V.Picture
  -- ^ The current vty output. 'runVtyAppWith' samples this value every time an
  -- event fires and updates the display.
  , _vtyResult_shutdown :: Event t ()
  -- ^ An event that requests application termination.
  }

type MonadVtyApp t m =
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , PrimMonad (HostFrame t)
  , ReflexHost t
  , MonadIO (HostFrame t)
  , Ref m ~ IORef
  , Ref (HostFrame t) ~ IORef
  , MonadRef (HostFrame t)
  , NotReady t m
  , TriggerEvent t m
  , PostBuild t m
  , PerformEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  )

-- | A functional reactive vty application.
type VtyApp t m = MonadVtyApp t m
  => DisplayRegion
  -- ^ The initial display size (updates to this come as events)
  -> Event t (V.Event)
  -- ^ Vty input events.
  -> m (VtyResult t)
  -- ^ The output of the 'VtyApp'. The application runs in a context that,
  -- among other things, allows new events to be created and triggered
  -- ('TriggerEvent'), provides access to an event that fires immediately upon
  -- app instantiation ('PostBuild'), and allows actions to be run upon
  -- occurrences of events ('PerformEvent').

-- | Runs a 'VtyApp' in a given 'Vty'.
runVtyAppWith
  :: V.Vty
  -- ^ A 'Vty' handle.
  -> (forall t m. VtyApp t m)
  -- ^ A functional reactive vty application.
  -> IO ()
runVtyAppWith vty vtyGuest =
  -- We are using the 'Spider' implementation of reflex. Running the host
  -- allows us to take actions on the FRP timeline. The scoped type signature
  -- specifies that our host runs on the Global timeline.
  -- For more information, see 'Reflex.Spider.Internal.runSpiderHost'.
  (runSpiderHost :: SpiderHost Global a -> IO a) $ do

    -- Create an 'Event' and a "trigger" reference for that event. The trigger
    -- reference can be used to determine whether anyone is "subscribed" to
    -- that 'Event' and, therefore, whether we need to bother performing any
    -- updates when the 'Event' fires.
    -- The 'Event' below will be used to convey vty input events.
    (vtyEvent, vtyEventTriggerRef) <- newEventWithTriggerRef

    -- Create the "post-build" event and associated trigger. This event fires
    -- once, when the application starts.
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef

    -- Create a queue to which we will write 'Event's that need to be
    -- processed.
    events <- liftIO newChan

    displayRegion0 <- V.displayBounds $ V.outputIface vty

    -- Run the vty "guest" application, providing the appropriate context. The
    -- result is a 'VtyResult', and a 'FireCommand' that will be used to
    -- trigger events.
    (vtyResult, fc@(FireCommand fire)) <- do
      hostPerformEventT $                 -- Allows the guest app to run
                                          -- 'performEvent', so that actions
                                          -- (e.g., IO actions) can be run when
                                          -- 'Event's fire.

        flip runPostBuildT postBuild $    -- Allows the guest app to access to
                                          -- a "post-build" 'Event'

          flip runTriggerEventT events $  -- Allows the guest app to create new
                                          -- events and triggers and writes
                                          -- those triggers to a channel from
                                          -- which they will be read and
                                          -- processed.

            vtyGuest displayRegion0 vtyEvent
                                          -- The guest app is provided the
                                          -- initial display region and an
                                          -- 'Event' of vty inputs.

    -- Reads the current value of the 'Picture' behavior and updates the
    -- display with it. This will be called whenever we determine that a
    -- display update is necessary. In this implementation that is when various
    -- events occur.
    let updateVty =
          sample (_vtyResult_picture vtyResult) >>= liftIO . V.update vty

    -- Read the trigger reference for the post-build event. This will be
    -- 'Nothing' if the guest application hasn't subscribed to this event.
    mPostBuildTrigger <- readRef postBuildTriggerRef

    -- When there is a subscriber to the post-build event, fire the event.
    forM_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ return ()

    -- After firing the post-build event, sample the vty result and update
    -- the display.
    updateVty

    -- Subscribe to an 'Event' of that the guest application can use to
    -- request application shutdown. We'll check whether this 'Event' is firing
    -- to determine whether to terminate.
    shutdown <- subscribeEvent $ _vtyResult_shutdown vtyResult

    -- Fork a thread and continuously get the next vty input event, and then
    -- write the input event to our channel of FRP 'Event' triggers.
    -- The thread is forked here because 'nextEvent' blocks.
    nextEventThread <- liftIO $ forkIO $ forever $ do
      -- Retrieve the next input event.
      ne <- V.nextEvent vty
      let -- The reference to the vty input 'EventTrigger'. This is the trigger
          -- we'd like to associate the input event value with.
          triggerRef = EventTriggerRef vtyEventTriggerRef
          -- Create an event 'TriggerInvocation' with the value that we'd like
          -- the event to have if it is fired. It may not fire with this value
          -- if nobody is subscribed to the 'Event'.
          triggerInvocation = TriggerInvocation ne $ return ()
      -- Write our input event's 'EventTrigger' with the newly created
      -- 'TriggerInvocation' value to the queue of events.
      writeChan events $ [triggerRef :=> triggerInvocation]

    -- The main application loop. We wait for new events, fire those that
    -- have subscribers, and update the display. If we detect a shutdown
    -- request, the application terminates.
    fix $ \loop -> do
      -- Read the next event (blocking).
      ers <- liftIO $ readChan events
      stop <- do
        -- Fire events that have subscribers.
        fireEventTriggerRefs fc ers $
          -- Check if the shutdown 'Event' is firing.
          readEvent shutdown >>= \case
            Nothing -> return False
            Just _ -> return True
      if or stop
        then liftIO $ do             -- If we received a shutdown 'Event'
          killThread nextEventThread -- then stop reading input events and
          V.shutdown vty             -- call the 'Vty's shutdown command.

        else do                      -- Otherwise, update the display and loop.
          updateVty
          loop
  where
    -- TODO Some part of this is probably general enough to belong in reflex
    -- | Use the given 'FireCommand' to fire events that have subscribers
    -- and call the callback for the 'TriggerInvocation' of each.
    fireEventTriggerRefs
      :: (Monad (ReadPhase m), MonadIO m)
      => FireCommand t m
      -> [DSum (EventTriggerRef t) TriggerInvocation]
      -> ReadPhase m a
      -> m [a]
    fireEventTriggerRefs (FireCommand fire) ers rcb = do
      mes <- liftIO $
        forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          return $ fmap (\e -> e :=> Identity a) me
      a <- fire (catMaybes mes) rcb
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
      return a

-- | Run a 'VtyApp' with a 'Vty' handle with a standard configuration.
runVtyApp
  :: (forall t m. VtyApp t m)
  -> IO ()
runVtyApp app = do
  cfg <- liftIO V.standardIOConfig
  vty <- liftIO $ V.mkVty $ cfg { V.mouseMode = Just True }
  runVtyAppWith vty app


---- Widget.hs

data Region = Region
  { _region_left :: Int
  , _region_top :: Int
  , _region_width :: Int
  , _region_height :: Int
  }
  deriving (Show, Read, Eq, Ord)

regionToDisplayRegion :: Region -> DisplayRegion
regionToDisplayRegion (Region _ _ w h) = (w, h)

region :: Region -> (DisplayRegion -> Image) -> [Image]
region r@(Region _ _ width height) i =
  [ within r $ V.resize width height $ i (width, height)
  , within r $ wrapString width V.defAttr $ replicate (width * height) ' '
  ]

within :: Region -> Image -> Image
within (Region left top width height) =
  V.translate left top . V.crop width height

wrapString :: Int -> Attr -> String -> Image
wrapString maxWidth attrs = V.vertCat . concatMap (fmap (V.string attrs) . fmap (take maxWidth) . takeWhile (not . null) . iterate (drop maxWidth)) . lines

box :: Region -> (DisplayRegion -> Image) -> [Image]
box r@(Region left top width height) i =
  let hBorder = V.string mempty $ replicate width '-'
      vBorder = wrapString 1 mempty $ replicate (height - 2) '|'
      internal = Region
        { _region_left = left + 1
        , _region_top = top + 1
        , _region_width = width - 2
        , _region_height = height - 2
        }
  in  [ within (r { _region_height = 1 }) hBorder
      , within (Region left (top + 1) 1 (height - 2)) vBorder
      , within (Region (left + width - 1) (top + 1) 1 (height - 2)) vBorder
      , within (r { _region_top = top + height - 1 }) hBorder
      ] ++ region internal i

class HasDisplaySize t m | m -> t where
  displaySize :: m (Dynamic t (Int, Int))

class HasVtyInput t m | m -> t where
  input :: m (Event t VtyEvent)

class HasFocus t m | m -> t where
  focus :: m (Dynamic t Bool)

data VtyWidgetCtx t = VtyWidgetCtx
  { _vtyWidgetCtx_size :: Dynamic t (Int,Int) -- ^ The width and height of the region allocated to the widget.
  , _vtyWidgetCtx_focus :: Dynamic t Bool -- ^ Whether the widget should behave as if it has focus for keyboard input.
  , _vtyWidgetCtx_input :: Event t VtyEvent -- ^ User input events that the widget's parent chooses to share. These will generally
                                            -- be filtered for relevance: 
                                            --  * Keyboard inputs are restricted to focused widgets
                                            --  * Mouse inputs are restricted to the region in which the widget resides.
  }

data VtyWidgetOut t = VtyWidgetOut
  { _vtyWidgetOut_images :: Behavior t [Image]
  , _vtyWidgetOut_shutdown :: Event t ()
  }

instance (Reflex t) => Semigroup (VtyWidgetOut t) where
  wo <> wo' = VtyWidgetOut
    { _vtyWidgetOut_images = _vtyWidgetOut_images wo <> _vtyWidgetOut_images wo'
    , _vtyWidgetOut_shutdown = _vtyWidgetOut_shutdown wo <> _vtyWidgetOut_shutdown wo'
    }

instance (Reflex t) => Monoid (VtyWidgetOut t) where
  mempty = VtyWidgetOut mempty mempty
  mappend wo wo' = wo <> wo'

newtype VtyWidget t m a = VtyWidget { unVtyWidget :: WriterT (VtyWidgetOut t) (ReaderT (VtyWidgetCtx t) m) a }
  deriving (Functor, Applicative, Monad, MonadSample t, MonadHold t)

runVtyWidget :: (MonadFix m, Reflex t)
  => VtyWidgetCtx t
  -> VtyWidget t m a
  -> m (a, VtyWidgetOut t)
runVtyWidget ctx w = runReaderT (runWriterT (unVtyWidget w)) ctx

mainVtyWidget :: (forall t m. MonadVtyApp t m => VtyWidget t m ()) -> IO ()
mainVtyWidget child =
  runVtyApp $ \dr0 inp -> do
    size <- holdDyn dr0 $ fforMaybe inp $ \case
      V.EvResize w h -> Just (w, h)
      _ -> Nothing
    let ctx = VtyWidgetCtx
          { _vtyWidgetCtx_size = size
          , _vtyWidgetCtx_input = inp
          , _vtyWidgetCtx_focus = constDyn True
          }
    ((), wo) <- runVtyWidget ctx child
    return $ VtyResult
      { _vtyResult_picture = fmap V.picForLayers (_vtyWidgetOut_images wo)
      , _vtyResult_shutdown = _vtyWidgetOut_shutdown wo
      }

modifyImages :: (Reflex t, MonadHold t m, MonadFix m)
  => Behavior t ([Image] -> [Image]) -> VtyWidget t m a -> VtyWidget t m a
modifyImages f (VtyWidget w) = VtyWidget $
  censor (\wo -> wo { _vtyWidgetOut_images = f <*> (_vtyWidgetOut_images wo) })
         w

instance (Reflex t, Monad m) => HasDisplaySize t (VtyWidget t m) where
  displaySize = VtyWidget . lift $ asks _vtyWidgetCtx_size

instance (Reflex t, Monad m) => HasVtyInput t (VtyWidget t m) where
  input = VtyWidget . lift $ asks _vtyWidgetCtx_input

instance (Reflex t, Monad m) => HasFocus t (VtyWidget t m) where
  focus = VtyWidget . lift $ asks _vtyWidgetCtx_focus

tellImages :: (Reflex t, Monad m) => Behavior t [Image] -> VtyWidget t m ()
tellImages imgs = VtyWidget $ tell (mempty { _vtyWidgetOut_images = imgs })

tellShutdown :: (Reflex t, Monad m) => Event t () -> VtyWidget t m ()
tellShutdown sd = VtyWidget $ tell (mempty { _vtyWidgetOut_shutdown = sd })

pane :: (MonadFix m, Reflex t)
     => Dynamic t Region -- ^ Region into which we should draw the widget (in coordinates relative to our own)
     -> Dynamic t Bool -- ^ Whether the widget is focused
     -> VtyWidget t m a
     -> VtyWidget t m a
pane reg foc w = VtyWidget $ do
  ctx <- lift ask
  let ctx' = VtyWidgetCtx
        { _vtyWidgetCtx_input = fmapMaybe id $
            attachWith (\(r,f) e -> filterInput r f e)
              (liftA2 (,) (current reg) (current foc))
              (_vtyWidgetCtx_input ctx)
        , _vtyWidgetCtx_focus = foc
        , _vtyWidgetCtx_size = fmap regionToDisplayRegion reg }
  (result, wo) <- lift . lift $ runVtyWidget ctx' w
  let images = _vtyWidgetOut_images wo
      images' = liftA2 (\r is -> map (within r) is) (current reg) images
      wo' = wo { _vtyWidgetOut_images = images' }
  tell wo'
  return result

filterInput :: Region -> Bool -> VtyEvent -> Maybe VtyEvent
filterInput r focused e = case e of
  V.EvKey _ _ | not focused -> Nothing
  V.EvMouseDown x y btn m -> mouse (\u v -> V.EvMouseDown u v btn m) x y
  V.EvMouseUp x y btn -> mouse (\u v -> V.EvMouseUp u v btn) x y
  _ -> Just e
  where
    mouse con x y
      | or [ x < 0
           , y < 0
           , x >= _region_width r
           , y >= _region_height r ] = Nothing
      | otherwise =
        Just (con (x + _region_left r) (y + _region_top r))
