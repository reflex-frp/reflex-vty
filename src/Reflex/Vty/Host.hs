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

import Control.Concurrent.Async (AsyncCancelled, async, cancel, link, waitCatch)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, catch, mask, throwIO, try)
import Control.Monad (forM, forM_, forever, void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef, Ref, readRef)
import Data.Default (Default (..))
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
--
-- The whole run is wrapped in one masked lifecycle. Resources are
-- acquired and recorded under masking. Exactly one cleanup path owns
-- teardown. Every exit path uses it: normal shutdown, guest failure,
-- rendering or input failure, or external cancellation. The cleanup
-- steps run in order. They close the event queue and release blocked
-- producers. They cancel and join the input worker if it was started.
-- They restore the OS signal handlers if they were installed. They
-- restore the default cursor style and the primary screen. They shut
-- Vty down exactly once. Every step is attempted even if an earlier
-- step failed. The application's own exception, if any, takes
-- precedence over cleanup failures.
runVtyAppWithHandle
  :: VtyAppConfig
  -- ^ Host configuration (event-queue capacity, etc.). Use
  -- 'defaultVtyAppConfig' for defaults.
  -> V.Vty
  -- ^ A 'Graphics.Vty.Vty' handle.
  -> (forall t m. VtyApp t m)
  -- ^ A functional reactive vty application.
  -> IO ()
runVtyAppWithHandle cfg vty vtyGuest = mask $ \restore -> do
  workerRef <- newIORef Nothing
  signalRestoreRef <- newIORef Nothing
  pending <- newBoundedEventQueue (fromIntegral (max 1 (_vtyConfig_eventQueueCapacity cfg)))
  appResult <-
    try $
      restore $
        (runSpiderHost :: SpiderHost Global () -> IO ()) $ do
          -- We are using the 'Spider' implementation of reflex. Running the host
          -- allows us to take actions on the FRP timeline. The scoped type signature
          -- specifies that our host runs on the Global timeline.
          -- For more information, see 'Reflex.Spider.Internal.runSpiderHost'.

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

          displayRegion0 <- liftIO $ V.displayBounds $ V.outputIface vty

          -- Run the vty "guest" application, providing the appropriate context.
          -- The result is a 'VtyResult', and a 'FireCommand' that will be used to
          -- trigger events. The guest runs in 'BoundedTriggerT' so that its
          -- 'TriggerEvent' methods (and hence 'Reflex.performEventAsync') route
          -- writes through the bounded queue above. A producer that fires
          -- faster than the host can process then blocks instead of leaking
          -- memory.
          (vtyResult, fc@(FireCommand fire)) <- do
            hostPerformEventT $ -- Allows the guest app to run
            -- 'performEvent', so that actions
            -- (e.g., IO actions) can be run when
            -- 'Event's fire.
              flip runPostBuildT postBuild $ -- Allows the guest app to access to
              -- a "post-build" 'Event'
                flip runBoundedTriggerT pending $
                  vtyGuest displayRegion0 vtyEvent signalEvent
          -- The guest app is provided the
          -- initial display region, an
          -- 'Event' of vty inputs, and an
          -- 'Event' of OS signals.

          -- Subscribe, before anything is fired or drawn, to the 'Event' the
          -- guest application can use to request shutdown. SIGINT, SIGTERM,
          -- and SIGHUP from the host also trigger shutdown. This early
          -- subscription means the frame that fires post-build observes a
          -- shutdown request made at post-build.
          let sigShutdown =
                () <$ ffilter (\s -> s == AppSignal_Interrupt || s == AppSignal_Terminate || s == AppSignal_Hangup) signalEvent
          shutdown <- subscribeEvent $ leftmost [_vtyResult_shutdown vtyResult, sigShutdown]
          let readShutdown =
                readEvent shutdown >>= \case
                  Nothing -> return False
                  Just _ -> return True

          -- Install OS signal handlers now. This happens after guest
          -- construction and shutdown subscription, but before firing
          -- post-build or drawing the first picture. Record how to restore
          -- the previous handlers. From this point on, a signal is enqueued
          -- and processed by the main loop below.
          liftIO $ do
            restoreSignals <-
              installAppSignalHandlers $ \appSignal ->
                atomically $
                  writeBoundedEventQueue pending [EventTriggerRef signalTriggerRef :=> TriggerInvocation appSignal (return ())]
            writeIORef signalRestoreRef (Just restoreSignals)

          -- Reads the current value of the 'Picture' behavior and updates the
          -- display with it. This will be called whenever we determine that a
          -- display update is necessary.
          let updateVty = sample (_vtyResult_picture vtyResult) >>= liftIO . V.update vty

          -- Read the trigger reference for the post-build event. This will be
          -- 'Nothing' if the guest application hasn't subscribed to this event.
          -- When firing it, the read phase checks the (already subscribed)
          -- shutdown event. Every frame result returned by @fire@ is
          -- examined, including follow-up frames from 'performEvent'
          -- actions. A shutdown requested at post-build therefore terminates
          -- here, before the input worker starts or an unnecessary frame is
          -- drawn.
          mPostBuildTrigger <- readRef postBuildTriggerRef
          stopAtPostBuild <- case mPostBuildTrigger of
            Nothing -> return False
            Just postBuildTrigger -> or <$> fire [postBuildTrigger :=> Identity ()] readShutdown

          if stopAtPostBuild
            then return ()
            else do
              -- Draw the first picture.
              updateVty

              -- Continuously read the next vty input event and enqueue it.
              -- nextEvent blocks, so this runs on its own thread. The worker
              -- is linked, so an unexpected input-reader failure is rethrown
              -- in this (host) thread. Deliberate cancellation during
              -- teardown completes the worker normally and does not
              -- propagate.
              inputWorker <-
                liftIO $
                  async $
                    ( restore $ forever $ do
                        ne <- V.nextEvent vty
                        atomically $
                          writeBoundedEventQueue pending [EventTriggerRef vtyEventTriggerRef :=> TriggerInvocation ne (return ())]
                    )
                      `catch` \(_ :: AsyncCancelled) -> return ()
              liftIO $ link inputWorker
              liftIO $ writeIORef workerRef (Just inputWorker)

              -- The main application loop. We block until at least one batch
              -- of events is available, then drain every other batch that has
              -- accumulated in the meantime, fire each batch in its own
              -- Reflex frame, and redraw once.
              --
              -- Draining the whole queue each frame keeps the host from
              -- falling behind under bursts, while the queue's bounded
              -- capacity (see 'VtyAppConfig') backpressures a producer that
              -- fires faster than the host can process, bounding memory
              -- without dropping occurrences. Firing each batch in its own
              -- frame preserves every occurrence even when many firings of
              -- the same trigger land in one drain (Reflex collapses
              -- simultaneous same-trigger firings, so merging them into one
              -- frame would drop occurrences); drawing only once per drain
              -- decouples the display rate from the event rate.
              fix $ \loop -> do
                -- Block until at least one batch is available, then
                -- atomically drain every other batch that has accumulated.
                mBatches <- liftIO $ restore $ drainBoundedEventQueue pending
                case mBatches of
                  -- Queue was closed and empty (e.g. another thread
                  -- triggered shutdown); stop the loop.
                  Nothing -> return ()
                  Just batches -> do
                    -- Fire each batch in its own frame, stopping early if
                    -- the shutdown event fires in any frame. This includes
                    -- follow-up frames from 'performEvent' actions.
                    let fireUntilShutdown [] = return False
                        fireUntilShutdown (b : bs) = do
                          stops <- fireEventTriggerRefs fc b readShutdown
                          if or stops then return True else fireUntilShutdown bs
                    stop <- fireUntilShutdown batches
                    if stop
                      then return ()
                      else do
                        updateVty
                        loop

  -- Exactly one cleanup path owns teardown. Each step is attempted even
  -- if a previous step failed. The application's own exception, if any,
  -- takes precedence over the first cleanup failure.
  firstCleanupFailure <- do
    let steps =
          [ closeBoundedEventQueue pending
          , do
              mw <- readIORef workerRef
              forM_ mw $ \w -> cancel w >> void (waitCatch w)
          , do
              mr <- readIORef signalRestoreRef
              forM_ mr id
          , setCursorStyle (V.outputIface vty) CursorStyleDefault
          , setScreenMode (V.outputIface vty) ScreenNormal
          , V.shutdown vty
          ]
    results <- mapM (\act -> try act :: IO (Either SomeException ())) steps
    return $ case [e | Left e <- results] of
      (e : _) -> Just e
      [] -> Nothing
  case (appResult, firstCleanupFailure) of
    (Left appErr, _) -> throwIO (appErr :: SomeException)
    (Right (), Just cleanupErr) -> throwIO (cleanupErr :: SomeException)
    (Right (), Nothing) -> return ()
  where
    -- \| Use the given 'FireCommand' to fire events that have subscribers
    -- and call the callback for the 'TriggerInvocation' of each. The
    -- completion callback of every /processed/ batch runs exactly once,
    -- whether or not its trigger has a subscriber. Batches dropped after
    -- closure, and batches never drained, do not run callbacks.
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
