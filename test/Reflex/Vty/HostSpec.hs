{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Description: Host event-channel regression + backpressure tests.
module Reflex.Vty.HostSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes.Color (ColorMode (..))
import Reflex
import System.Timeout (timeout)
import Test.Hspec

import Reflex.Vty.Host
  ( VtyApp
  , VtyResult (..)
  , defaultVtyAppConfig
  , runVtyAppWithHandle
  )

----------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------

-- | Burst size for the regression test: large enough that the old
-- one-batch-per-frame host loop would not reach the shutdown request within
-- 'shutdownTimeout' (it would need one redraw per item).
burstN :: Int
burstN = 10000

-- | Burst size for the occurrence-preservation test. Deliberately far larger
-- than the default queue capacity (4096) so that backpressure provably engages
-- and the producer is throttled rather than able to dump everything at once.
overloadN :: Int
overloadN = 50000

-- | Time the host is allowed to take. The fixed host drains the whole burst in
-- a single frame, so this is ample; the old host (one batch per frame) would
-- need @burstN * redrawDelay@ microseconds and time out.
shutdownTimeout :: Int
shutdownTimeout = 6 * 1000000

-- | Generous timeout for the occurrence-preservation test: under backpressure
-- the producer is throttled to the host's processing rate, so @overloadN@
-- ticks take a little while to drain.
overloadTimeout :: Int
overloadTimeout = 30 * 1000000

-- | Per-redraw delay modelled by the mock vty. Simulates the cost of a real
-- 'V.update'.
redrawDelay :: Int
redrawDelay = 2000

spec :: Spec
spec = describe "Reflex.Vty.Host" $ do
  describe "runVtyAppWithHandle" $ do
    it "drains a burst of external triggers each frame and does not leak (regression)" $ do
      vty <- mockVty (\_ -> threadDelay redrawDelay)
      result <- timeout shutdownTimeout $ runVtyAppWithHandle defaultVtyAppConfig vty guest
      -- The host must shut down within the timeout. With the old loop it would
      -- stall behind @burstN@ backlogged batches (one redraw each) and time out.
      result `shouldBe` Just ()

    it "preserves every occurrence of an overloaded external trigger (backpressure)" $ do
      -- A producer firing faster than the host can process must be backpressured
      -- (no dropped occurrences) rather than allowed to exhaust memory.
      counter <- newIORef (0 :: Int)
      vty <- mockVty (\_ -> threadDelay redrawDelay)
      result <- timeout overloadTimeout $ runVtyAppWithHandle defaultVtyAppConfig vty (countingGuest counter)
      result `shouldBe` Just ()
      final <- readIORef counter
      -- Every one of the @overloadN@ ticks must be counted: backpressure throttles
      -- the producer but never drops an occurrence.
      final `shouldBe` overloadN

----------------------------------------------------------------------------
-- Guests
----------------------------------------------------------------------------

-- | Fires a burst of events into the FRP network (exactly the pattern that
-- backed up the host's event channel) and then requests shutdown. Both the
-- ticks and the shutdown request travel through the same 'TriggerEvent'
-- channel, so the only way the host reaches shutdown promptly is by draining
-- the whole channel each frame.
guest :: VtyApp t m
guest _region _input _sigs = do
  setupE <- getPostBuild
  (tickE, fireTick) <- newTriggerEvent
  (shutdownE, fireShutdown) <- newTriggerEvent
  -- Subscribe to the ticks so each occurrence does real work in the network.
  _d <- holdDyn (0 :: Int) tickE
  performEvent_ $
    ffor setupE $ \() ->
      liftIO $
        void $
          forkIO $ do
            replicateM_ burstN (fireTick (0 :: Int))
            fireShutdown ()
  return $
    VtyResult
      { _vtyResult_picture = pure (V.picForImage V.emptyImage)
      , _vtyResult_shutdown = shutdownE
      }

-- | Like 'guest' but increments an 'IORef' on every tick occurrence, so the
-- test can assert that no occurrences were dropped under backpressure.
countingGuest :: IORef Int -> (forall t m. VtyApp t m)
countingGuest counter _region _input _sigs = do
  setupE <- getPostBuild
  (tickE, fireTick) <- newTriggerEvent
  (shutdownE, fireShutdown) <- newTriggerEvent
  performEvent_ $ ffor tickE $ \_ -> liftIO $ modifyIORef' counter (+ 1)
  performEvent_ $
    ffor setupE $ \() ->
      liftIO $
        void $
          forkIO $ do
            replicateM_ overloadN (fireTick (0 :: Int))
            fireShutdown ()
  return $
    VtyResult
      { _vtyResult_picture = pure (V.picForImage V.emptyImage)
      , _vtyResult_shutdown = shutdownE
      }

----------------------------------------------------------------------------
-- Mock vty (no real terminal)
----------------------------------------------------------------------------

-- | A 'V.Vty' whose 'update' action runs the given @IO@ (to model redraw
-- cost) and whose 'nextEvent' blocks forever; the network is driven entirely
-- via 'TriggerEvent'. Only the fields the host actually touches are real;
-- the rest are never forced (the mock's @update@ never dereferences the
-- unused 'V.Output' fields).
mockVty :: (V.Picture -> IO ()) -> IO V.Vty
mockVty updateAction = do
  out <- mockOutput
  pure
    V.Vty
      { V.update = updateAction
      , V.nextEvent = forever (threadDelay 1000000)
      , V.nextEventNonblocking = pure Nothing
      , V.inputIface = mockInput
      , V.outputIface = out
      , V.refresh = pure ()
      , V.shutdown = pure ()
      , V.isShutdown = pure False
      }

mockInput :: V.Input
mockInput =
  V.Input
    { V.eventChannel = error "mockInput: eventChannel unused"
    , V.shutdownInput = pure ()
    , V.restoreInputState = pure ()
    , V.inputLogMsg = \_ -> pure ()
    }

mockOutput :: IO V.Output
mockOutput =
  pure
    V.Output
      { V.terminalID = "mock"
      , V.releaseTerminal = pure ()
      , V.reserveDisplay = pure ()
      , V.releaseDisplay = pure ()
      , V.setDisplayBounds = \_ -> pure ()
      , V.displayBounds = pure (80, 24)
      , V.outputByteBuffer = \_ -> pure ()
      , V.supportsCursorVisibility = True
      , V.supportsMode = const False
      , V.setMode = \_ _ -> pure ()
      , V.getModeStatus = \_ -> pure False
      , V.assumedStateRef = error "mockOutput: assumedStateRef unused"
      , V.mkDisplayContext = \_ _ -> error "mockOutput: mkDisplayContext unused"
      , V.ringTerminalBell = pure ()
      , V.supportsBell = pure True
      , V.supportsItalics = pure True
      , V.supportsStrikethrough = pure True
      , V.outputColorMode = FullColor
      , V.setOutputWindowTitle = \_ -> pure ()
      }
