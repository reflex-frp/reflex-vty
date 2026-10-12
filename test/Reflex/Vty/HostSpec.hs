{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description: Host lifecycle, backpressure, and failure semantics.
--
-- All scenarios run the real 'Reflex.Vty.Host.runVtyAppWithHandle'
-- against the recording terminal from "Reflex.Vty.Test.AppHarness".
-- Host tests are serial because signal handlers are process-global.
module Reflex.Vty.HostSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (Async, cancel, waitCatch)
import Control.Concurrent.MVar
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM_, replicateM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (isInfixOf)
import qualified Graphics.Vty as V
import Reflex
import System.Timeout (timeout)
import Test.Hspec

#if !defined(mingw32_HOST_OS)
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR1)
#endif

import Reflex.Vty.Host
import Reflex.Vty.Test.AppHarness
  ( RecordingVtyConfig (..)
  , defaultRecordingVtyConfig
  , recordingLogHistory
  , recordingReleaseGate
  , recordingVty
  , runHostAsync
  )

-- | Burst for the drain regression: far beyond what a
-- one-redraw-per-batch loop could finish inside 'defaultDeadline'.
burstN :: Int
burstN = 10000

-- | Occurrence-preservation burst, deliberately far beyond the queue
-- capacity used in that test so backpressure provably engages.
overloadN :: Int
overloadN = 50000

-- | Deadlines (microseconds).
defaultDeadline :: Int
defaultDeadline = 10 * 1000000

slowDeadline :: Int
slowDeadline = 30 * 1000000

-- | Count exact log-marker occurrences.
countIn :: String -> [String] -> Int
countIn tag logs = length [() | l <- logs, l == tag]

-- | An empty-picture result that shuts down on the given event.
emptyPictureResult :: Reflex t => Event t () -> VtyResult t
emptyPictureResult shutdown =
  VtyResult
    { _vtyResult_picture = constant (V.picForImage V.emptyImage)
    , _vtyResult_shutdown = shutdown
    }

-- | Build a guest with an external tick trigger and a stop trigger.
-- Hand both fire functions to the test through the 'MVar' during
-- guest construction. The result builder receives the tick and stop
-- events.
triggerGuest
  :: MVar (() -> IO () -> IO (), () -> IO () -> IO ())
  -> (forall t. Reflex t => Event t () -> Event t () -> VtyResult t)
  -> (forall t m. VtyApp t m)
triggerGuest ready mkResult _dr _inp _sigs = do
  (tickE, fireTick) <- newTriggerEventWithOnComplete
  (stopE, fireStop) <- newTriggerEventWithOnComplete
  -- Subscribe to the tick so occurrences do real frame work.
  performEvent_ $ ffor tickE $ \() -> pure ()
  liftIO $ putMVar ready (fireTick, fireStop)
  return (mkResult tickE stopE)

-- | Expect the host to finish successfully within the deadline;
-- cancel it and fail with diagnostics otherwise.
expectHostOk :: Async () -> [String] -> IO ()
expectHostOk hostA logs = do
  r <- timeout slowDeadline (waitCatch hostA)
  case r of
    Nothing -> do
      cancel hostA
      expectationFailure
        ( "host did not terminate within deadline; log tail: "
            ++ show (drop (max 0 (length logs - 40)) logs)
        )
    Just (Left e) ->
      expectationFailure
        ( "host failed: "
            ++ show e
            ++ "; log tail: "
            ++ show (drop (max 0 (length logs - 40)) logs)
        )
    Just (Right ()) -> pure ()

spec :: Spec
spec = describe "Reflex.Vty.Host" $ do
  describe "runVtyAppWithHandle" $ do
    it "drains a burst of external triggers in bounded frames (regression)" $ do
      -- The old host drew once per queued batch and could not reach
      -- shutdown inside the deadline; the draining host redraws once
      -- per drain.
      ready <- newEmptyMVar
      let cfg = defaultVtyAppConfig
          guest :: VtyApp t m
          guest = triggerGuest ready (\_tick stop -> emptyPictureResult stop)
      (vty, st) <- recordingVty defaultRecordingVtyConfig
      hostA <- runHostAsync cfg vty guest
      (fireTick, fireStop) <- takeMVar ready
      _ <- forkIO $ replicateM_ burstN (fireTick () (pure ())) >> fireStop () (pure ())
      logsAfter <- recordingLogHistory st
      expectHostOk hostA logsAfter
      logs <- recordingLogHistory st
      countIn "draw" logs `shouldSatisfy` (< 100)

    it "preserves every occurrence of an overloaded external trigger (backpressure)" $ do
      counter <- newIORef (0 :: Int)
      ready <- newEmptyMVar
      let cfg = defaultVtyAppConfig {_vtyConfig_eventQueueCapacity = 16}
          guest :: VtyApp t m
          guest = triggerGuest ready (\_tick stop -> emptyPictureResult stop)
      (vty, st) <- recordingVty defaultRecordingVtyConfig
      hostA <- runHostAsync cfg vty guest
      (fireTick, fireStop) <- takeMVar ready
      _ <- forkIO $ do
        replicateM_ overloadN $ fireTick () (atomicModifyIORef' counter (\i -> (i + 1, ())))
        fireStop () (pure ())
      logsAfter <- recordingLogHistory st
      expectHostOk hostA logsAfter
      readIORef counter `shouldReturn` overloadN

    it "drains a gated, capacity-filling burst with bounded redraws" $ do
      ready <- newEmptyMVar
      let cfg = defaultVtyAppConfig {_vtyConfig_eventQueueCapacity = 10001}
          guest :: VtyApp t m
          guest = triggerGuest ready (\_tick stop -> emptyPictureResult stop)
      (vty, st) <- recordingVty defaultRecordingVtyConfig {recordingGateRedraw = True}
      hostA <- runHostAsync cfg vty guest
      (fireTick, fireStop) <- takeMVar ready
      -- The initial redraw is gated. Enqueue the entire finite burst
      -- and the shutdown: 10,001 writes fit the 10,001-capacity queue,
      -- so the producer cannot deadlock against the gate.
      forM_ [1 .. 10000 :: Int] $ \_ -> fireTick () (pure ())
      fireStop () (pure ())
      recordingReleaseGate st
      logsAfter <- recordingLogHistory st
      expectHostOk hostA logsAfter
      logs <- recordingLogHistory st
      countIn "draw" logs `shouldSatisfy` (<= 3)

    it "terminates immediately on post-build shutdown without drawing" $ do
      -- Regression: post-build shutdown used to be lost (the shutdown
      -- subscription happened after the post-build fire), leaving the
      -- host blocked on input forever.
      let guest :: VtyApp t m
          guest _dr _inp _sigs = do
            pb <- getPostBuild
            return (emptyPictureResult pb)
      (vty, st) <- recordingVty defaultRecordingVtyConfig
      hostA <- runHostAsync defaultVtyAppConfig vty guest
      r <- timeout defaultDeadline (waitCatch hostA)
      case r of
        Nothing -> cancel hostA >> expectationFailure "host did not terminate at post-build shutdown"
        Just (Left e) -> expectationFailure ("host failed: " ++ show e)
        Just (Right ()) -> pure ()
      logs <- recordingLogHistory st
      countIn "draw" logs `shouldBe` 0 -- no unnecessary frame was drawn
    it "terminates immediately when a post-build perform-event requests shutdown" $ do
      let guest :: VtyApp t m
          guest _dr _inp _sigs = do
            (stopE, fireStop) <- newTriggerEventWithOnComplete
            pb <- getPostBuild
            performEvent_ $ ffor pb $ \_ -> liftIO (fireStop () (pure ()))
            return (emptyPictureResult (() <$ stopE))
      (vty, st) <- recordingVty defaultRecordingVtyConfig
      hostA <- runHostAsync defaultVtyAppConfig vty guest
      r <- timeout defaultDeadline (waitCatch hostA)
      case r of
        Nothing -> cancel hostA >> expectationFailure "host did not terminate on perform-event shutdown"
        Just (Left e) -> expectationFailure ("host failed: " ++ show e)
        Just (Right ()) -> pure ()
      logs <- recordingLogHistory st
      -- The stop request travels through the external-trigger queue.
      -- The host therefore draws at most the initial picture, never
      -- starts the input pump, and runs no interactive frame.
      countIn "draw" logs `shouldSatisfy` (<= 1)

    it "runs processed trigger callbacks exactly once, without subscribers" $ do
      callbacks <- newIORef (0 :: Int)
      ready <- newEmptyMVar
      let guest :: VtyApp t m
          guest _dr _inp _sigs = do
            (fireE, fire) <- newTriggerEventWithOnComplete
            (stopE, fireStop) <- newTriggerEventWithOnComplete
            -- Nobody subscribes to fireE; its occurrences must still
            -- run their completion callbacks exactly once each when
            -- processed.
            liftIO $ putMVar ready (fire, fireStop)
            return (emptyPictureResult (() <$ stopE))
      (vty, _) <- recordingVty defaultRecordingVtyConfig
      hostA <- runHostAsync defaultVtyAppConfig vty guest
      (fire, fireStop) <- takeMVar ready
      let bump = atomicModifyIORef' callbacks (\i -> (i + 1, ()))
      fire () bump
      fire () bump
      fire () bump
      fireStop () (pure ())
      r <- timeout defaultDeadline (waitCatch hostA)
      case r of
        Nothing -> cancel hostA >> expectationFailure "host did not terminate"
        Just (Left e) -> expectationFailure ("host failed: " ++ show e)
        Just (Right ()) -> pure ()
      readIORef callbacks `shouldReturn` 3

    describe "failure scenarios" $ do
      it "propagates guest construction failure and shuts vty down once" $ do
        let guest :: VtyApp t m
            guest _dr _inp _sigs = liftIO (ioError (userError "guest-boom"))
        (vty, st) <- recordingVty defaultRecordingVtyConfig
        r <- timeout defaultDeadline (try (runVtyAppWithHandle defaultVtyAppConfig vty guest) :: IO (Either SomeException ()))
        case r of
          Nothing -> expectationFailure "host did not fail fast"
          Just (Left e) -> show e `shouldContain` "guest-boom"
          Just (Right ()) -> expectationFailure "expected the guest failure to propagate"
        logs <- recordingLogHistory st
        countIn "vty:shutdown" logs `shouldBe` 1

      it "propagates initial update failure" $ do
        ready <- newEmptyMVar
        let guest :: VtyApp t m
            guest = triggerGuest ready (\_tick _stop -> emptyPictureResult never)
        (vty, st) <-
          recordingVty
            defaultRecordingVtyConfig
              { recordingOnPicture = \_ -> throwIO (userError "update-boom")
              }
        hostA <- runHostAsync defaultVtyAppConfig vty guest
        r <- timeout defaultDeadline (waitCatch hostA)
        case r of
          Nothing -> cancel hostA >> expectationFailure "host did not fail fast"
          Just (Left e) -> show e `shouldContain` "update-boom"
          Just (Right ()) -> expectationFailure "expected the update failure to propagate"
        logs <- recordingLogHistory st
        countIn "vty:shutdown" logs `shouldBe` 1

      it "propagates a later update failure" $ do
        ready <- newEmptyMVar
        drawRef <- newIORef (0 :: Int)
        let guest :: VtyApp t m
            guest = triggerGuest ready (\_tick stop -> emptyPictureResult stop)
        (vty, st) <-
          recordingVty
            defaultRecordingVtyConfig
              { recordingOnPicture = \_ -> do
                  n <- atomicModifyIORef' drawRef (\i -> (i + 1, i + 1))
                  when (n >= 2) $ throwIO (userError "later-update-boom")
              }
        hostA <- runHostAsync defaultVtyAppConfig vty guest
        (fireTick, _) <- takeMVar ready
        -- One tick forces the next drain's redraw. The guest never
        -- shuts down, so the host must make that redraw (and its
        -- failure) happen.
        fireTick () (pure ())
        r <- timeout defaultDeadline (waitCatch hostA)
        case r of
          Nothing -> cancel hostA >> expectationFailure "host did not fail fast"
          Just (Left e) -> show e `shouldContain` "later-update-boom"
          Just (Right ()) -> expectationFailure "expected the later update failure to propagate"
        logs <- recordingLogHistory st
        countIn "vty:shutdown" logs `shouldBe` 1

      it "propagates input-reader failure" $ do
        let guest :: VtyApp t m
            guest _dr _inp _sigs = return (emptyPictureResult never)
        (vty0, st) <- recordingVty defaultRecordingVtyConfig
        let vty = vty0 {V.nextEvent = throwIO (userError "input-boom")}
        hostA <- runHostAsync defaultVtyAppConfig vty guest
        r <- timeout defaultDeadline (waitCatch hostA)
        case r of
          Nothing -> cancel hostA >> expectationFailure "input failure did not propagate"
          Just (Left e) -> show e `shouldContain` "input-boom"
          Just (Right ()) -> expectationFailure "expected input failure to fail the host"
        logs <- recordingLogHistory st
        countIn "vty:shutdown" logs `shouldBe` 1

      it "propagates perform-event failure" $ do
        let guest :: VtyApp t m
            guest _dr _inp _sigs = do
              pb <- getPostBuild
              performEvent_ $ ffor pb $ \_ -> liftIO (throwIO (userError "perform-boom"))
              return (emptyPictureResult never)
        (vty, st) <- recordingVty defaultRecordingVtyConfig
        r <- timeout defaultDeadline (try (runVtyAppWithHandle defaultVtyAppConfig vty guest) :: IO (Either SomeException ()))
        case r of
          Nothing -> expectationFailure "host did not fail fast"
          Just (Left e) -> show e `shouldContain` "perform-boom"
          Just (Right ()) -> expectationFailure "expected perform-event failure to propagate"
        logs <- recordingLogHistory st
        countIn "vty:shutdown" logs `shouldBe` 1

      it "releases a backpressured producer on external cancellation" $ do
        producerDone <- newEmptyMVar
        ready <- newEmptyMVar
        let cfg = defaultVtyAppConfig {_vtyConfig_eventQueueCapacity = 1}
            guest :: VtyApp t m
            guest = triggerGuest ready (\_tick stop -> emptyPictureResult stop)
        (vty, st) <- recordingVty defaultRecordingVtyConfig
        hostA <- runHostAsync cfg vty guest
        (fireTick, _) <- takeMVar ready
        _ <- forkIO $ do
          -- Fires far more than the capacity: it backpressures until
          -- cancellation closes the queue, then finishes.
          replicateM_ 100 (fireTick () (pure ()))
          putMVar producerDone ()
        threadDelay 200000 -- let the producer hit the full queue
        cancel hostA
        done <- timeout defaultDeadline (takeMVar producerDone)
        done `shouldBe` Just ()
        _ <- waitCatch hostA
        logs <- recordingLogHistory st
        countIn "vty:shutdown" logs `shouldBe` 1

      it "prefers the application failure over a cleanup failure, still running later cleanup" $ do
        let guest :: VtyApp t m
            guest _dr _inp _sigs = liftIO (ioError (userError "app-boom"))
        (vty0, st) <- recordingVty defaultRecordingVtyConfig
        let vty =
              vty0
                { V.outputIface =
                    (V.outputIface vty0)
                      { V.outputByteBuffer = \_ -> throwIO (userError "cleanup-boom")
                      }
                }
        r <- timeout defaultDeadline (try (runVtyAppWithHandle defaultVtyAppConfig vty guest) :: IO (Either SomeException ()))
        case r of
          Nothing -> expectationFailure "host did not fail fast"
          Just (Left e) -> do
            show e `shouldContain` "app-boom"
            show e `shouldNotContain` "cleanup-boom"
          Just (Right ()) -> expectationFailure "expected the application failure"
        -- Later cleanup steps still ran despite the cursor/screen
        -- restore failing.
        logs <- recordingLogHistory st
        countIn "vty:shutdown" logs `shouldBe` 1

      it "attempts cursor and screen restoration on normal shutdown" $ do
        ready <- newEmptyMVar
        let guest :: VtyApp t m
            guest = triggerGuest ready (\_tick stop -> emptyPictureResult stop)
        (vty, st) <- recordingVty defaultRecordingVtyConfig
        hostA <- runHostAsync defaultVtyAppConfig vty guest
        (_, fireStop) <- takeMVar ready
        fireStop () (pure ())
        r <- timeout defaultDeadline (waitCatch hostA)
        case r of
          Nothing -> cancel hostA >> expectationFailure "host did not terminate"
          Just (Left e) -> expectationFailure ("host failed: " ++ show e)
          Just (Right ()) -> pure ()
        logs <- recordingLogHistory st
        -- The primary-screen restore emits escape sequence 1049l
        -- through the output byte buffer.
        any ("?1049l" `isInfixOf`) logs `shouldBe` True
        countIn "vty:shutdown" logs `shouldBe` 1

#if !defined(mingw32_HOST_OS)
      it "restores prior signal handlers on shutdown" $ do
        priorCount <- newIORef (0 :: Int)
        _ <- installHandler sigUSR1 (Catch (atomicModifyIORef' priorCount (\i -> (i + 1, ())))) Nothing
        ready <- newEmptyMVar
        -- The guest signals construction completion at post-build.
        -- The host installs signal handlers before post-build, so the
        -- handshake cannot race installation.
        let guest :: VtyApp t m
            guest _dr _inp _sigs = do
              pb <- getPostBuild
              performEvent_ $ ffor pb $ \_ -> liftIO (putMVar ready ())
              return (emptyPictureResult never)
        (vty, _) <- recordingVty defaultRecordingVtyConfig
        hostA <- runHostAsync defaultVtyAppConfig vty guest
        _ <- timeout defaultDeadline (takeMVar ready)
        cancel hostA
        _ <- waitCatch hostA
        -- Retrieve the currently active handler: it must be the prior
        -- counting handler again.
        active <- installHandler sigUSR1 (Catch (pure ())) Nothing
        case active of
          Catch act -> act
          _ -> expectationFailure "expected the prior handler to be restored"
        _ <- installHandler sigUSR1 active Nothing
        readIORef priorCount `shouldReturn` 1
#endif
