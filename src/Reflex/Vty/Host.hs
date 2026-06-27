-- |
-- Module: Reflex.Vty.Host
-- Description: Scaffolding for running a reflex-vty application
module Reflex.Vty.Host
  ( VtyApp
  , VtyResult (..)
  , VtyAppConfig (..)
  , defaultVtyAppConfig
  , CursorStyle (..)
  , CursorVisibility (..)
  , setCursorStyle
  , ScreenMode (..)
  , setScreenMode
  , AppSignal (..)
  , getDefaultVty
  , runVtyApp
  , runVtyAppWithHandle
  , MonadVtyApp
  , VtyEvent
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Exception (onException)
import Control.Monad (forM, forM_, forever)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef, Ref, readRef)
import Data.Default (Default (..))
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (IORef, readIORef)
import Data.Maybe (catMaybes)
import Graphics.Vty (DisplayRegion)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Reflex
import Reflex.Host.Class

import Reflex.Vty.Host.Signal
  ( AppSignal (..)
  , installAppSignalHandlers
  )
import Reflex.Vty.Host.Trigger
  ( closeBoundedEventQueue
  , drainBoundedEventQueue
  , newBoundedEventQueue
  , runBoundedTriggerT
  , writeBoundedEventQueue
  )

-- | A synonym for the underlying vty event type from 'Graphics.Vty'. This should
-- probably ultimately be replaced by something defined in this library.
type VtyEvent = V.Event

-- | The output of a 'VtyApp'.
data VtyResult t = VtyResult
  { _vtyResult_picture :: Behavior t V.Picture
  -- ^ The current vty output. 'runVtyAppWithHandle' samples this value every time an
  --   event fires and updates the display.
  , _vtyResult_shutdown :: Event t ()
  -- ^ An event that requests application termination.
  }

-- | The constraints necessary to run a 'VtyApp'. See 'runVtyAppWithHandle' for more
-- on why each of these are necessary and how they can be fulfilled.
type MonadVtyApp t m =
  ( Reflex t
  , Adjustable t m
  , MonadCatch m
  , MonadFix (Performable m)
  , MonadFix m
  , MonadHold t (Performable m)
  , MonadHold t m
  , MonadIO (HostFrame t)
  , MonadIO (Performable m)
  , MonadIO m
  , MonadMask m
  , MonadRef (HostFrame t)
  , MonadThrow m
  , NotReady t m
  , PerformEvent t m
  , PostBuild t m
  , PrimMonad (HostFrame t)
  , Ref (HostFrame t) ~ IORef
  , Ref m ~ IORef
  , ReflexHost t
  , TriggerEvent t m
  )

-- | A functional reactive vty application.
type VtyApp t m =
  MonadVtyApp t m
  => DisplayRegion
  -- ^ The initial display size (updates to this come as events)
  -> Event t V.Event
  -- ^ Vty input events.
  -> Event t AppSignal
  -- ^ OS signal events, normalized across platforms. 'AppSignal_Interrupt',
  -- 'AppSignal_Terminate', and 'AppSignal_Hangup' all trigger shutdown.
  -- NOTE: This behavior will be configurable in the future.
  -> m (VtyResult t)
  -- ^ The output of the 'VtyApp'. The application runs in a context that,
  --   among other things, allows new events to be created and triggered
  --   ('TriggerEvent'), provides access to an event that fires immediately upon
  --   app instantiation ('PostBuild'), and allows actions to be run upon
  --   occurrences of events ('PerformEvent').

-- | Configuration for running a 'VtyApp'.
data VtyAppConfig = VtyAppConfig
  { _vtyConfig_eventQueueCapacity :: !Int
  -- ^ Maximum number of pending external trigger invocations the host will
  -- buffer before backpressuring producers (e.g. a hot
  -- 'Reflex.performEventAsync' callback). When the buffer is full, a
  -- producer's @fire@ blocks until the host catches up, bounding memory
  -- without dropping any occurrences. See 'defaultVtyAppConfig'.
  }

-- | A sensible default 'VtyAppConfig': an event-queue capacity of 4096, which
-- is large enough to absorb any realistic burst (a large paste, a flappy
-- mouse, network callbacks) without throttling, while keeping worst-case
-- per-frame fire work and memory modest (~530 KB ceiling).
instance Default VtyAppConfig where
  def = VtyAppConfig {_vtyConfig_eventQueueCapacity = 4096}

-- | The default 'VtyAppConfig' (identical to 'def').
defaultVtyAppConfig :: VtyAppConfig
defaultVtyAppConfig = def

-- | Runs a 'VtyApp' in a given 'Graphics.Vty.Vty'.
runVtyAppWithHandle
  :: VtyAppConfig
  -- ^ Host configuration (event-queue capacity, etc.). Use
  -- 'defaultVtyAppConfig' for defaults.
  -> V.Vty
  -- ^ A 'Graphics.Vty.Vty' handle.
  -> (forall t m. VtyApp t m)
  -- ^ A functional reactive vty application.
  -> IO ()
runVtyAppWithHandle cfg vty vtyGuest = flip onException (V.shutdown vty) $
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

    -- Create an 'Event' for OS signals.
    (signalEvent, signalTriggerRef) <- newEventWithTriggerRef

    -- A bounded, closeable queue into which external triggers write their
    -- pending invocations. Boundedness gives us backpressure (a producer that
    -- fires faster than the host can process has its @fire@ block when the
    -- queue is full), which bounds memory without dropping occurrences. The
    -- guest runs in 'BoundedTriggerT' below so that its 'TriggerEvent'
    -- methods (and hence 'Reflex.performEventAsync') route writes through
    -- this queue.
    pending <-
      liftIO $
        newBoundedEventQueue (fromIntegral (max 1 (_vtyConfig_eventQueueCapacity cfg)))

    displayRegion0 <- liftIO $ V.displayBounds $ V.outputIface vty

    -- Run the vty "guest" application, providing the appropriate context. The
    -- result is a 'VtyResult', and a 'FireCommand' that will be used to
    -- trigger events.
    (vtyResult, fc@(FireCommand fire)) <- do
      hostPerformEventT $ -- Allows the guest app to run
      -- 'performEvent', so that actions
      -- (e.g., IO actions) can be run when
      -- 'Event's fire.
        flip runPostBuildT postBuild $ -- Allows the guest app to access to
        -- a "post-build" 'Event'
          flip runBoundedTriggerT pending $ -- Allows the guest app to create new
          -- events and triggers; writes route
          -- through the bounded queue above so
          -- that hot producers backpressure
          -- instead of leaking.
            vtyGuest displayRegion0 vtyEvent signalEvent
    -- The guest app is provided the
    -- initial display region, an
    -- 'Event' of vty inputs, and an
    -- 'Event' of OS signals.

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
    -- to determine whether to terminate. SIGINT, SIGTERM, and SIGHUP from the
    -- host also trigger shutdown.
    let sigShutdown = () <$ ffilter (\s -> s == AppSignal_Interrupt || s == AppSignal_Terminate || s == AppSignal_Hangup) signalEvent
    shutdown <- subscribeEvent $ leftmost [_vtyResult_shutdown vtyResult, sigShutdown]

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
      -- 'TriggerInvocation' value to the queue of events. (Like all external
      -- triggers, this is subject to backpressure if the queue is full; in
      -- practice input never saturates it.)
      atomically $ writeBoundedEventQueue pending [triggerRef :=> triggerInvocation]

    -- Install OS signal handlers.
    liftIO $ installAppSignalHandlers $ \appSignal ->
      atomically $ writeBoundedEventQueue pending [EventTriggerRef signalTriggerRef :=> TriggerInvocation appSignal (return ())]

    -- The main application loop. We block until at least one batch of events
    -- is available, then drain every other batch that has accumulated in the
    -- meantime, fire each batch in its own Reflex frame, and redraw once.
    --
    -- Draining the whole queue each frame keeps the host from falling behind
    -- under bursts, while the queue's bounded capacity (see 'VtyAppConfig')
    -- backpressures a producer that fires faster than the host can process,
    -- bounding memory without dropping occurrences. Firing each batch in its
    -- own frame preserves every occurrence even when many firings of the same
    -- trigger land in one drain (Reflex collapses simultaneous same-trigger
    -- firings, so merging them into one frame would drop occurrences); drawing
    -- only once per drain decouples the display rate from the event rate.
    fix $ \loop -> do
      -- Block until at least one batch is available, then atomically drain
      -- every other batch that has accumulated.
      mBatches <- liftIO $ drainBoundedEventQueue pending
      case mBatches of
        -- Queue was closed and empty (e.g. another thread triggered
        -- shutdown); stop the loop.
        Nothing -> return ()
        Just batches -> do
          -- Fire each batch in its own frame, stopping early if the shutdown
          -- event fires.
          let fireUntilShutdown [] = return False
              fireUntilShutdown (b : bs) = do
                stops <-
                  fireEventTriggerRefs fc b $
                    readEvent shutdown >>= \case
                      Nothing -> return False
                      Just _ -> return True
                if or stops then return True else fireUntilShutdown bs
          stop <- fireUntilShutdown batches
          if stop
            then liftIO $ do
              -- If we received a shutdown 'Event', close the queue first so
              -- any producer blocked on a full queue is released, then stop
              -- reading input, restore the primary screen, and shut vty down.
              closeBoundedEventQueue pending
              killThread nextEventThread
              setScreenMode (V.outputIface vty) ScreenNormal
              V.shutdown vty
            else do
              -- Otherwise, update the display and loop.
              updateVty
              loop
  where
    -- \| Use the given 'FireCommand' to fire events that have subscribers
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

-- | Run a 'VtyApp' with a 'Graphics.Vty.Vty' handle with a standard configuration.
runVtyApp
  :: VtyAppConfig
  -> (forall t m. VtyApp t m)
  -> IO ()
runVtyApp cfg app = do
  vty <- getDefaultVty
  runVtyAppWithHandle cfg vty app

-- | Terminal cursor shape. Not all terminals support all styles; the
-- fallback is always a block cursor. Set via DECSCUSR escape sequences
-- (not part of vty 6.2's API).
data CursorStyle
  = -- | Restore the terminal's default cursor shape.
    CursorStyleDefault
  | -- | Steady block cursor.
    CursorStyleBlock
  | -- | Steady underline cursor.
    CursorStyleUnderline
  | -- | Steady vertical bar cursor (xterm extension).
    CursorStyleBar
  | CursorStyleBlinkingBlock
  | CursorStyleSteadyBlock
  | CursorStyleBlinkingUnderline
  | CursorStyleSteadyUnderline
  | -- | Blinking vertical bar (xterm extension).
    CursorStyleBlinkingBar
  | -- | Steady vertical bar (xterm extension).
    CursorStyleSteadyBar
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Whether the terminal cursor should be rendered.
data CursorVisibility
  = CursorVisible
  | CursorHidden
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Set the terminal cursor shape by emitting DECSCUSR escape sequences
-- directly to the output byte buffer.
setCursorStyle :: V.Output -> CursorStyle -> IO ()
setCursorStyle out style =
  V.outputByteBuffer out (toSeq style)
  where
    toSeq CursorStyleDefault = "\ESC[0 q"
    toSeq CursorStyleBlock = "\ESC[2 q"
    toSeq CursorStyleUnderline = "\ESC[4 q"
    toSeq CursorStyleBar = "\ESC[6 q"
    toSeq CursorStyleBlinkingBlock = "\ESC[1 q"
    toSeq CursorStyleSteadyBlock = "\ESC[2 q"
    toSeq CursorStyleBlinkingUnderline = "\ESC[3 q"
    toSeq CursorStyleSteadyUnderline = "\ESC[4 q"
    toSeq CursorStyleBlinkingBar = "\ESC[5 q"
    toSeq CursorStyleSteadyBar = "\ESC[6 q"

-- | Which screen buffer to use. Most full-screen TUI apps use the
-- alternate screen so the terminal restores prior content on exit.
data ScreenMode
  = ScreenNormal
  | ScreenAlternate
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Enter or exit the alternate screen buffer by emitting DECSET/DECRST
-- escape sequences (1049) directly to the output byte buffer.
setScreenMode :: V.Output -> ScreenMode -> IO ()
setScreenMode out = \case
  ScreenNormal -> V.outputByteBuffer out "\ESC[?1049l"
  ScreenAlternate -> V.outputByteBuffer out "\ESC[?1049h"

-- | Returns the standard vty configuration with mouse, focus tracking,
-- and bracketed paste enabled.
getDefaultVty :: IO V.Vty
getDefaultVty = do
  cfg <- V.userConfig
  vty <- V.mkVty cfg
  liftIO $ do
    V.setMode (V.outputIface vty) V.Mouse True
    V.setMode (V.outputIface vty) V.Focus True
    V.setMode (V.outputIface vty) V.BracketedPaste True
  return vty
