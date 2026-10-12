{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description: Bounded trigger-queue semantics.
--
-- Batch payloads are existentially typed, so these queue-level checks
-- assert counts and states. The production host proves true
-- first-in-first-out /ordering/ in "Reflex.Vty.HostSpec".
module Reflex.Vty.Host.TriggerSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forM)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef
import Reflex
import Reflex.Spider (Spider)
import System.Timeout (timeout)
import Test.Hspec

import Reflex.Vty.Host.Trigger

-- | A synthetic batch. The trigger ref points at an empty IORef (no
-- subscriber), which is enough for queue-level semantics.
mkBatch :: Int -> IO (Batch Spider)
mkBatch _n = do
  r <- newIORef Nothing
  pure [EventTriggerRef r :=> TriggerInvocation () (pure ())]

-- | A batch whose completion callback appends to the reference.
mkBatchWithCallback :: IORef [Int] -> Int -> IO (Batch Spider)
mkBatchWithCallback logRef n = do
  r <- newIORef Nothing
  pure
    [ EventTriggerRef r
        :=> TriggerInvocation () (atomicModifyIORef' logRef (\ls -> (ls ++ [n], ())))
    ]

-- | Would the write commit immediately (True) or block (False)?
probeWrite :: BoundedEventQueue Spider -> Batch Spider -> IO Bool
probeWrite q batch =
  atomically ((writeBoundedEventQueue q batch >> pure True) `orElse` pure False)

-- | A deadline for these fast, deterministic checks (microseconds).
fastDeadline :: Int
fastDeadline = 5 * 1000000

-- | Expect a drain to yield batches, with the count checked.
expectDrained :: Maybe (Maybe [Batch Spider]) -> Int -> Expectation
expectDrained result n = case result of
  Nothing -> expectationFailure "drain did not finish within the deadline"
  Just Nothing ->
    expectationFailure ("expected " ++ show n ++ " drained batches, got the closed-and-empty end")
  Just (Just batches) -> length batches `shouldBe` n

-- | The drain blocked (its deadline expired without a result).
expectBlocked :: Maybe (Maybe [Batch Spider]) -> Expectation
expectBlocked Nothing = pure ()
expectBlocked (Just r) =
  expectationFailure
    ("expected the drain to block, but it returned " ++ maybe "the closed end" (\bs -> show (length bs) ++ " batches") r)

-- | The drain finished and reported the closed-and-empty end.
expectClosedAndEmpty :: Maybe (Maybe [Batch Spider]) -> Expectation
expectClosedAndEmpty result = case result of
  Nothing -> expectationFailure "drain did not finish within the deadline"
  Just Nothing -> pure ()
  Just (Just batches) ->
    expectationFailure
      ("expected the closed-and-empty end, but drained " ++ show (length batches) ++ " batches")

spec :: Spec
spec = describe "Reflex.Vty.Host.Trigger" $ do
  describe "newBoundedEventQueue" $ do
    it "clamps capacity 0 to one slot" $ do
      q <- newBoundedEventQueue 0
      b1 <- mkBatch 1
      b2 <- mkBatch 2
      ok1 <- probeWrite q b1
      ok1 `shouldBe` True
      ok2 <- probeWrite q b2
      ok2 `shouldBe` False -- single slot, now full
    it "accepts several batches and blocks when full" $ do
      q <- newBoundedEventQueue 4
      bs <- forM [1 .. 4] mkBatch
      results <- mapM (probeWrite q) bs
      results `shouldBe` [True, True, True, True]
      b5 <- mkBatch 5
      ok5 <- probeWrite q b5
      ok5 `shouldBe` False -- backpressure: the queue is full
      drained <- timeout fastDeadline (drainBoundedEventQueue q)
      expectDrained drained 4
      -- The queue is empty and still open: draining blocks again.
      immediate <- timeout 10000 (drainBoundedEventQueue q)
      expectBlocked immediate

  describe "drainBoundedEventQueue" $ do
    it "waits on an open empty queue, then completes after enqueue and close" $ do
      q <- newBoundedEventQueue 2
      resultVar <- newEmptyTMVarIO
      _ <- forkIO $ drainBoundedEventQueue q >>= atomically . putTMVar resultVar
      -- The drain is still blocked.
      notYet <- atomically $ (Just <$> readTMVar resultVar) `orElse` pure Nothing
      expectBlocked notYet
      b <- mkBatch 42
      atomically $ writeBoundedEventQueue q b
      closeBoundedEventQueue q
      r <- timeout fastDeadline (atomically (readTMVar resultVar))
      case r of
        Just (Just batches) -> length batches `shouldBe` 1
        _ -> expectationFailure "drain did not complete after enqueue and close"

    it "drains queued batches despite closure, then reports the end" $ do
      q <- newBoundedEventQueue 4
      bs <- forM [7, 8] mkBatch
      mapM_ (atomically . writeBoundedEventQueue q) bs
      closeBoundedEventQueue q
      first <- timeout fastDeadline (drainBoundedEventQueue q)
      expectDrained first 2
      second <- timeout fastDeadline (drainBoundedEventQueue q)
      expectClosedAndEmpty second

  describe "closeBoundedEventQueue" $ do
    it "is idempotent" $ do
      q <- newBoundedEventQueue 1
      closeBoundedEventQueue q
      closeBoundedEventQueue q
      r <- timeout fastDeadline (drainBoundedEventQueue q)
      expectClosedAndEmpty r

    it "refuses writes even with spare capacity (closed-first check)" $ do
      q <- newBoundedEventQueue 8
      b <- mkBatch 1
      atomically $ writeBoundedEventQueue q b
      closeBoundedEventQueue q
      -- Seven slots are free, but the closed queue must refuse the
      -- write: it commits as a no-op rather than enqueueing.
      ok <- probeWrite q b
      ok `shouldBe` True
      r <- timeout fastDeadline (drainBoundedEventQueue q)
      expectDrained r 1 -- only the pre-close batch
    it "releases a producer blocked on a full queue without enqueueing" $ do
      q <- newBoundedEventQueue 1
      b1 <- mkBatch 1
      b2 <- mkBatch 2
      atomically $ writeBoundedEventQueue q b1
      committedVar <- newEmptyTMVarIO
      _ <- forkIO $ do
        -- Blocks until the write commits (or the queue closes).
        atomically (writeBoundedEventQueue q b2)
        atomically (putTMVar committedVar ())
      -- The producer is still blocked.
      notYet <- atomically $ (Just <$> readTMVar committedVar) `orElse` pure Nothing
      notYet `shouldBe` Nothing
      closeBoundedEventQueue q
      -- Block (do not probe) until the released producer completes.
      done <- timeout fastDeadline (atomically (Just <$> readTMVar committedVar))
      done `shouldBe` Just (Just ())
      -- The released write must not have enqueued.
      r <- timeout fastDeadline (drainBoundedEventQueue q)
      expectDrained r 1

    it "drops post-close writes without running their completion callbacks" $ do
      q <- newBoundedEventQueue 1
      cbLog <- newIORef ([] :: [Int])
      closeBoundedEventQueue q
      b <- mkBatchWithCallback cbLog 1
      ok <- probeWrite q b
      ok `shouldBe` True
      r <- timeout fastDeadline (drainBoundedEventQueue q)
      expectClosedAndEmpty r
      readIORef cbLog `shouldReturn` []

  describe "producer/drain completion handshake" $ do
    it "drains everything a joined producer wrote, then ends" $ do
      q <- newBoundedEventQueue 3
      let n = 20 :: Int
      producerDone <- newEmptyTMVarIO
      _ <- forkIO $ do
        bs <- forM [1 .. n] mkBatch
        mapM_ (atomically . writeBoundedEventQueue q) bs
        closeBoundedEventQueue q
        atomically $ putTMVar producerDone ()
      _ <- timeout fastDeadline (atomically (readTMVar producerDone))
      let drainAll count = do
            r <- timeout fastDeadline (drainBoundedEventQueue q)
            case r of
              -- The deadline is not part of the contract; treat it as
              -- the end so a regression surfaces as a count mismatch.
              Nothing -> pure count
              Just Nothing -> pure count
              Just (Just batches) -> drainAll (count + length batches)
      total <- drainAll 0
      total `shouldBe` n
