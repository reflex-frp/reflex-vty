{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description: A deterministic harness around the production widget host
--
-- This module runs real widget applications through
-- 'Reflex.Vty.Widget.mainWidgetWithHandle' against a fully implemented
-- recording 'V.Vty'. It never recreates the widget transformer stack.
--
-- Determinism comes from /fences/. Every injected terminal input is
-- followed by a reserved marker event ('V.KFun' with a negative
-- argument, which terminals cannot produce). Every injected command is
-- followed by a control message. The widget tree observes the
-- marker's arrival and stamps it onto the next drawn frame. A step
-- therefore returns exactly the frame in which its input or command
-- took effect. Markers never reach the application under test. All
-- waits are STM-based and race against host failure, so a dying host
-- can never hang a test. Every step has a deadline. Timeouts report
-- the outstanding fence, recent frames, and the output\/lifecycle
-- log.
module Reflex.Vty.Test.AppHarness
  ( -- * Widget applications under test
    TestWidget
  , TestApp
  , Frame (..)
  , AppDriver (..)
  , withApp
  , awaitFrameWhereFor

    -- * Failures
  , HarnessTimeout (..)
  , HostFailure (..)
  , UnexpectedOperation (..)

    -- * Lower-level host access
  , RecordingVtyConfig (..)
  , defaultRecordingVtyConfig
  , runHostAsync
  , recordingVty
  , RecordingState (..)
  , recordingLog
  , recordingLogHistory
  , recordingInput
  , recordingSetBounds
  , recordingReleaseGate
  ) where

import Control.Concurrent.Async
  ( Async
  , async
  , cancel
  , poll
  , waitCatch
  , waitCatchSTM
  )
import Control.Concurrent.STM
  ( STM
  , TMVar
  , TQueue
  , TVar
  , atomically
  , check
  , modifyTVar'
  , newEmptyTMVarIO
  , newTMVarIO
  , newTQueueIO
  , newTVarIO
  , readTMVar
  , readTQueue
  , readTVar
  , readTVarIO
  , retry
  , tryReadTQueue
  , writeTQueue
  , writeTVar
  )
import Control.Exception (Exception, SomeException, catch, displayException, throwIO, try)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.List (intercalate)
import GHC.Conc (unsafeIOToSTM)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes.Color (ColorMode (..))
import Reflex
import System.Timeout (timeout)

import Control.Monad.NodeId (MonadNodeId)
import Reflex.Vty.Host (MonadVtyApp, VtyApp, VtyAppConfig)
import qualified Reflex.Vty.Host as Host
import Reflex.Vty.Test.Snapshot (Grid, pictureToGrid)
import "reflex-vty" Reflex.Vty.Widget

-- | Exactly the constraint set 'Reflex.Vty.Widget.mainWidgetWithHandle'
-- accepts for its child widget. This is a constraint alias only: the
-- application under test runs in the production widget stack.
type TestWidget t m =
  ( MonadVtyApp t m
  , MonadNodeId m
  , HasImageWriter t m
  , HasDisplayRegion t m
  , HasFocusReader t m
  , HasInput t m
  , HasTheme t m
  , HasColorProfile t m
  , HasCursor t m
  , HasScreenMode t m
  )

-- | A widget application under test: it receives an event of external
-- commands (fired by 'stepCommand') and returns its current
-- observation together with a shutdown event.
type TestApp command observation =
  forall t m. TestWidget t m => Event t command -> m (Dynamic t observation, Event t ())

-- | One drawn frame: what the terminal actually received.
data Frame observation = Frame
  { frameNumber :: !Int
  -- ^ Monotonically increasing draw counter.
  , frameFence :: !Word
  -- ^ The fence stamped into this frame (0 = none yet).
  , frameViewport :: !V.DisplayRegion
  -- ^ The root viewport the application actually rendered with.
  , frameGrid :: Grid
  -- ^ The rendered picture, normalized through the independent oracle.
  , frameCursor :: V.Cursor
  -- ^ The picture's cursor request.
  , framePicture :: V.Picture
  -- ^ The raw picture, for assertion diagnostics.
  , frameObservation :: observation
  -- ^ The application's observed value at draw time.
  }

-- | The handle for driving one running application.
data AppDriver command observation = AppDriver
  { stepInput :: V.Event -> IO (Frame observation)
  -- ^ Deliver a terminal input event and return the frame in which it
  -- took effect.
  , stepCommand :: command -> IO (Frame observation)
  -- ^ Fire an external command and return the frame in which it took
  -- effect.
  , resizeApp :: V.DisplayRegion -> IO (Frame observation)
  -- ^ Change the terminal size, run the production resize debounce,
  -- and return the first frame rendered at the requested size.
  , awaitFrameWhere :: (Frame observation -> Bool) -> IO (Frame observation)
  -- ^ Block (STM) until a /new/ frame satisfies the predicate.
  , driverAwaitFor :: Int -> (Frame observation -> Bool) -> IO (Frame observation)
  -- ^ 'awaitFrameWhere' with an explicit microsecond deadline.
  , stopApp :: IO ()
  -- ^ Request shutdown and wait for the host to finish.
  , driverFrames :: IO [Frame observation]
  -- ^ All frames recorded so far, oldest first.
  , driverOutputLog :: IO [String]
  -- ^ Ordered input, control, output-byte, mode-change, and shutdown
  -- trace.
  , driverReleaseRedrawGate :: IO ()
  -- ^ Release the redraw gate (a no-op unless the recorder was created
  -- with gating enabled).
  , driverHostResult :: IO (Maybe (Either SomeException ()))
  -- ^ 'Nothing' while the host runs; otherwise its final result.
  }

-- | 'awaitFrameWhere' with an explicit microsecond deadline.
awaitFrameWhereFor
  :: AppDriver command observation -> Int -> (Frame observation -> Bool) -> IO (Frame observation)
awaitFrameWhereFor = driverAwaitFor

-- | Default per-operation deadline (microseconds).
defaultDeadline :: Int
defaultDeadline = 10 * 1000000

-- | Deadline for sustained-trigger scenarios, including the production
-- resize debounce and scrollbar quiet period (microseconds).
slowDeadline :: Int
slowDeadline = 30 * 1000000

-- | A step deadline expired.
newtype HarnessTimeout = HarnessTimeout String
  deriving (Show)

instance Exception HarnessTimeout

-- | The host failed while a step was waiting.
newtype HostFailure = HostFailure String
  deriving (Show)

instance Exception HostFailure

----------------------------------------------------------------------------
-- Recording Vty
----------------------------------------------------------------------------

-- | The recording terminal throws this for operations that no
-- supported host action performs.
newtype UnexpectedOperation = UnexpectedOperation String
  deriving (Show)

instance Exception UnexpectedOperation

-- | Configuration for the recording terminal.
data RecordingVtyConfig = RecordingVtyConfig
  { recordingColorMode :: V.ColorMode
  -- ^ Reported terminal color mode ('FullColor' by default).
  , recordingGateRedraw :: Bool
  -- ^ When true, every redraw blocks until 'recordingReleaseGate'.
  , recordingInitialBounds :: V.DisplayRegion
  , recordingOnPicture :: V.Picture -> IO ()
  -- ^ Called on every redraw with the drawn picture.
  }

-- | A sensible default: full color, no gating, 80x24, no observer.
defaultRecordingVtyConfig :: RecordingVtyConfig
defaultRecordingVtyConfig =
  RecordingVtyConfig
    { recordingColorMode = FullColor
    , recordingGateRedraw = False
    , recordingInitialBounds = (80, 24)
    , recordingOnPicture = \_ -> pure ()
    }

-- | The mutable state behind the recording terminal.
data RecordingState = RecordingState
  { rsInputQueue :: TQueue V.Event
  , rsBoundsRef :: IORef V.DisplayRegion
  , rsGate :: TVar Bool
  , rsLogRef :: IORef [String]
  }

-- | Append to the ordered operation log (oldest first when read back).
recordingLog :: RecordingState -> String -> IO ()
recordingLog st msg = atomicModifyIORef' (rsLogRef st) (\acc -> (msg : acc, ()))

-- | Read the full ordered operation log.
recordingLogHistory :: RecordingState -> IO [String]
recordingLogHistory st = reverse <$> readIORef (rsLogRef st)

-- | Enqueue a terminal input event.
recordingInput :: RecordingState -> V.Event -> IO ()
recordingInput st e = do
  recordingLog st ("input: " ++ show e)
  atomically $ writeTQueue (rsInputQueue st) e

-- | Change the terminal size reported by the recorder.
recordingSetBounds :: RecordingState -> V.DisplayRegion -> IO ()
recordingSetBounds st b = do
  recordingLog st ("resize: " ++ show b)
  writeIORef (rsBoundsRef st) b

-- | Release the redraw gate (a no-op when ungated).
recordingReleaseGate :: RecordingState -> IO ()
recordingReleaseGate st = atomically (writeTVar (rsGate st) True)

-- | Build a fully implemented recording 'V.Vty' plus its state. Every
-- operation is real; operations unreachable through initialization,
-- rendering, mode changes, and teardown throw 'UnexpectedOperation'
-- rather than diverging.
recordingVty :: RecordingVtyConfig -> IO (V.Vty, RecordingState)
recordingVty cfg = do
  inputQueue <- newTQueueIO
  boundsRef <- newIORef (recordingInitialBounds cfg)
  gate <-
    if recordingGateRedraw cfg
      then newTVarIO False
      else newTVarIO True
  logRef <- newIORef ([] :: [String])
  let st =
        RecordingState
          { rsInputQueue = inputQueue
          , rsBoundsRef = boundsRef
          , rsGate = gate
          , rsLogRef = logRef
          }
      log msg = recordingLog st msg
      updateAction pic = do
        -- Blocking on a closed latch gates redraws in backpressure
        -- tests; an ungated latch returns immediately. The latch is
        -- level-triggered: every redraw waits until it is open.
        atomically (readTVar gate >>= check)
        log "draw"
        recordingOnPicture cfg pic
      input =
        V.Input
          { V.eventChannel = error "recordingVty: eventChannel unused (nextEvent is used)"
          , V.shutdownInput = log "input:shutdown"
          , V.restoreInputState = log "input:restore"
          , V.inputLogMsg = \_ -> pure ()
          }
  out <- recordingOutput cfg log boundsRef
  let vty =
        V.Vty
          { V.update = updateAction
          , V.nextEvent = atomically (readTQueue inputQueue)
          , V.nextEventNonblocking = atomically (tryReadTQueue inputQueue)
          , V.inputIface = input
          , V.outputIface = out
          , V.refresh = log "refresh"
          , V.shutdown = log "vty:shutdown"
          , V.isShutdown = pure False
          }
  pure (vty, st)

recordingOutput :: RecordingVtyConfig -> (String -> IO ()) -> IORef V.DisplayRegion -> IO V.Output
recordingOutput cfg log boundsRef =
  pure output
  where
    output =
      V.Output
        { V.terminalID = "reflex-vty-test-recorder"
        , V.releaseTerminal = log "output:releaseTerminal"
        , V.reserveDisplay = log "output:reserveDisplay"
        , V.releaseDisplay = log "output:releaseDisplay"
        , V.setDisplayBounds = \r -> log ("output:setDisplayBounds " ++ show r)
        , V.displayBounds = readIORef boundsRef
        , V.outputByteBuffer = \bs -> log ("output:bytes[" ++ show (BS.length bs) ++ "] " ++ show bs)
        , V.supportsCursorVisibility = True
        , V.supportsMode = const True
        , V.setMode = \m onOff -> log ("output:setMode " ++ show (m, onOff))
        , V.getModeStatus = \m -> do
            log ("output:getModeStatus " ++ show m)
            pure False
        , V.assumedStateRef = error "recordingVty: assumedStateRef unreachable"
        , V.mkDisplayContext = \_ _ ->
            throwIO (UnexpectedOperation "mkDisplayContext: the recorder draws no display context")
        , V.ringTerminalBell = log "output:bell"
        , V.supportsBell = pure True
        , V.supportsItalics = pure True
        , V.supportsStrikethrough = pure True
        , V.outputColorMode = recordingColorMode cfg
        , V.setOutputWindowTitle = \t -> log ("output:title " ++ show t)
        }

-- | Run the production host asynchronously. For startup-shutdown and
-- failure-injection scenarios that must not wait for a first frame.
runHostAsync :: VtyAppConfig -> V.Vty -> (forall t m. VtyApp t m) -> IO (Async ())
runHostAsync cfg vty app = async (Host.runVtyAppWithHandle cfg vty app)

----------------------------------------------------------------------------
-- Fence markers and control messages
----------------------------------------------------------------------------

-- | The reserved marker event encoding fence @n@. Terminals cannot
-- deliver negative function-key arguments, so these never collide with
-- real input.
markerEvent :: Word -> V.Event
markerEvent n = V.EvKey (V.KFun (negate (fromIntegral n))) []

markerFenceOf :: V.Event -> Maybe Word
markerFenceOf e = case e of
  V.EvKey (V.KFun n) [] | n < 0 -> Just (fromIntegral (negate n))
  _ -> Nothing

dropMarker :: V.Event -> Maybe V.Event
dropMarker e = case markerFenceOf e of
  Just _ -> Nothing
  Nothing -> Just e

-- | Control-stream messages. The application only ever sees
-- 'CtrlCommand' payloads.
data Ctrl c = CtrlCommand c | CtrlFence Word | CtrlStop

isCtrlStop :: Ctrl c -> Bool
isCtrlStop CtrlStop = True
isCtrlStop _ = False

ctrlCommand :: Ctrl c -> Maybe c
ctrlCommand (CtrlCommand c) = Just c
ctrlCommand _ = Nothing

ctrlFence :: Ctrl c -> Maybe Word
ctrlFence (CtrlFence n) = Just n
ctrlFence _ = Nothing

----------------------------------------------------------------------------
-- Frame waiting
----------------------------------------------------------------------------

-- | Block (STM) until a frame newer than the baseline satisfies the
-- predicate. The wait races against host failure. A deadline expiry
-- throws 'HarnessTimeout' with diagnostics.
awaitNewFrame
  :: forall observation
   . TVar [Frame observation]
  -> IORef (Maybe (Async ()))
  -> IO [String]
  -> Int
  -> Int
  -> (Frame observation -> Bool)
  -> IO (Frame observation)
awaitNewFrame framesVar hostRef logs deadline baseline p = do
  r <- timeout deadline (atomically stm)
  case r of
    Nothing -> timeoutFailure
    Just (Left hostErr) -> do
      details <- diagnostics
      throwIO
        ( HostFailure $
            "host failed while waiting for a frame: "
              ++ hostErr
              ++ "\n"
              ++ details
        )
    Just (Right f) -> pure f
  where
    stm :: STM (Either String (Frame observation))
    stm = do
      fs <- readTVar framesVar
      case [f | f <- drop baseline fs, p f] of
        (f : _) -> pure (Right f)
        [] -> do
          mh <- unsafeIOToSTM (readIORef hostRef)
          case mh of
            Just h -> do
              r <- waitCatchSTM h
              case r of
                Left e -> pure (Left (displayException e))
                Right _ -> retry
            Nothing -> retry
    diagnostics = do
      fs <- readTVarIO framesVar
      logLines <- logs
      pure $
        "frames="
          ++ show (length fs)
          ++ " baseline="
          ++ show baseline
          ++ "\nlast frames:\n"
          ++ intercalate "\n" (map describeFrame (drop (max 0 (length fs - 5)) fs))
          ++ "\nlog (tail):\n"
          ++ intercalate "\n" (drop (max 0 (length logLines - 30)) logLines)
    timeoutFailure = do
      details <- diagnostics
      throwIO (HarnessTimeout ("harness timeout after " ++ show deadline ++ "us\n" ++ details))
    describeFrame f =
      "frame#"
        ++ show (frameNumber f)
        ++ " fence="
        ++ show (frameFence f)
        ++ " viewport="
        ++ show (frameViewport f)
        ++ " cursor="
        ++ show (frameCursor f)

----------------------------------------------------------------------------
-- withApp
----------------------------------------------------------------------------

-- | Run a 'TestApp' against a recording terminal of the given initial
-- size. Hand the driver to the continuation. The harness awaits the
-- initial frame before the continuation runs. When the continuation
-- returns (or throws), the harness shuts down and joins the host.
withApp
  :: forall command observation a
   . Show observation
  => VtyAppConfig
  -> V.DisplayRegion
  -> TestApp command observation
  -> (AppDriver command observation -> IO a)
  -> IO a
withApp cfg size app k = do
  framesVar <- newTVarIO ([] :: [Frame observation]) -- oldest first
  obsBug <- newIORef (0 :: Int)
  frameCounter <- newIORef (0 :: Int)
  fenceCounter <- newIORef (0 :: Word)
  lastFenceRef <- newIORef (0 :: Word)
  obsRef <- newIORef (Nothing :: Maybe observation)
  regionRef <- newIORef size
  ctrlRef <- newIORef (Nothing :: Maybe (Ctrl command -> IO () -> IO ()))
  hostRef <- newIORef (Nothing :: Maybe (Async ()))

  (vty, st) <-
    recordingVty
      defaultRecordingVtyConfig
        { recordingInitialBounds = size
        , recordingOnPicture = \pic -> do
            n <- atomicModifyIORef' frameCounter (\i -> (i + 1, i + 1))
            fence <- readIORef lastFenceRef
            vp <- readIORef regionRef
            mobs <- readIORef obsRef
            case mobs of
              Nothing ->
                modifyIORef' obsBug (+ 1)
              Just o ->
                atomically $
                  modifyTVar' framesVar (++ [Frame n fence vp (pictureToGrid vp pic) (V.picCursor pic) pic o])
        }
  let stampFence n = writeIORef lastFenceRef n
      nextFence = atomicModifyIORef' fenceCounter (\i -> (i + 1, i + 1))
      fireCtrl msg k' = do
        mfire <- readIORef ctrlRef
        case mfire of
          Nothing -> throwIO (UnexpectedOperation "control stream not yet initialized")
          Just fire -> fire msg k'
      awaitNew deadline baseline p =
        awaitNewFrame framesVar hostRef (recordingLogHistory st) deadline baseline p

      wrapper :: forall t m. TestWidget t m => m (Event t ())
      wrapper = do
        inp <- input
        pb <- getPostBuild
        -- Track the root viewport the application actually uses
        -- (after the production resize debounce).
        vp <- askViewport
        performEvent_ $
          ffor (leftmost [tag (current vp) pb, updated vp]) $ \r ->
            liftIO $ writeIORef regionRef (_region_width r, _region_height r)
        -- External command stream.
        (ctrl, fireCtrlRef) <- newTriggerEventWithOnComplete
        liftIO $ writeIORef ctrlRef (Just fireCtrlRef)
        -- The application under test: markers filtered out of its
        -- input; commands from the control stream.
        (obsDyn, appShutdown) <-
          localInput (fmapMaybe dropMarker) $
            app (fmapMaybe ctrlCommand ctrl)
        -- Observation stamping: initial value at post-build, then
        -- every update, so each draw sees the settled value.
        performEvent_ $
          ffor (leftmost [tag (current obsDyn) pb, updated obsDyn]) $
            liftIO . writeIORef obsRef . Just
        -- Fences: control-stream fences and input markers.
        performEvent_ $ ffor (fmapMaybe ctrlFence ctrl) (liftIO . stampFence)
        performEvent_ $ ffor (fmapMaybe markerFenceOf inp) (liftIO . stampFence)
        let stop = () <$ ffilter isCtrlStop ctrl
        return $ leftmost [appShutdown, stop]

  hostA <- async (mainWidgetWithHandle cfg vty wrapper)
  writeIORef hostRef (Just hostA)

  let awaitFrameFenced n = do
        fs0 <- readTVarIO framesVar
        awaitNew defaultDeadline (length fs0) ((== n) . frameFence)
      stepInputImpl ev = do
        n <- nextFence
        recordingInput st ev
        recordingInput st (markerEvent n)
        awaitFrameFenced n
      stepCommandImpl c = do
        n <- nextFence
        fireCtrl (CtrlCommand c) (pure ())
        fireCtrl (CtrlFence n) (pure ())
        awaitFrameFenced n
      resizeImpl dims = do
        recordingSetBounds st dims
        n <- nextFence
        recordingInput st (uncurry V.EvResize dims)
        recordingInput st (markerEvent n)
        _ <- awaitFrameFenced n
        fs0 <- readTVarIO framesVar
        awaitNew slowDeadline (length fs0) ((== dims) . frameViewport)
      stopImpl = do
        mfire <- readIORef ctrlRef
        forM_ mfire $ \fire ->
          fire CtrlStop (pure ()) `catch` \(_ :: SomeException) -> pure ()
        joinHost
      joinHost = do
        mh <- readIORef hostRef
        forM_ mh $ \h -> do
          pr <- poll h
          case pr of
            Just _ -> pure ()
            Nothing -> do
              r <- timeout defaultDeadline (waitCatch h)
              case r of
                Nothing -> cancel h >> void (waitCatch h)
                Just _ -> pure ()

      driver =
        AppDriver
          { stepInput = stepInputImpl
          , stepCommand = stepCommandImpl
          , resizeApp = resizeImpl
          , awaitFrameWhere = \p -> do
              fs0 <- readTVarIO framesVar
              awaitNew defaultDeadline (length fs0) p
          , driverAwaitFor = \deadline p -> do
              fs0 <- readTVarIO framesVar
              awaitNew deadline (length fs0) p
          , stopApp = stopImpl
          , driverFrames = readTVarIO framesVar
          , driverOutputLog = recordingLogHistory st
          , driverReleaseRedrawGate = recordingReleaseGate st
          , driverHostResult = do
              mh <- readIORef hostRef
              case mh of
                Nothing -> pure Nothing
                Just h -> poll h
          }

  -- The initial frame must exist before interaction begins (or the
  -- host must fail loudly rather than hang).
  _ <- awaitNew defaultDeadline 0 (const True)

  result <- try (k driver)
  stopImpl
  case result of
    Left e -> throwIO (e :: SomeException)
    Right a -> pure a
