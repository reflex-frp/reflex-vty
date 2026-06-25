{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Reflex.Vty.Host.Trigger
-- Description: A bounded 'TriggerEvent' layer that backpressures hot producers
--
-- This is an internal module used by "Reflex.Vty.Host". It provides a drop-in
-- replacement for reflex's @'Reflex.TriggerEvent.Base.TriggerEventT'@ whose
-- underlying queue is bounded and closeable. External triggers
-- ('Reflex.performEventAsync', 'Reflex.newTriggerEvent', etc.) write into a
-- fixed-capacity queue, so a producer that fires faster than the host can
-- process is throttled (its @fire@ callback blocks when the queue is full)
-- rather than allowed to exhaust memory. Because the producer's own write
-- blocks, no event occurrences are dropped.
--
-- The queue is also closeable: on shutdown the host closes it, which releases
-- any producer currently blocked on a full queue.
module Reflex.Vty.Host.Trigger
  ( -- * Bounded event queue
    BoundedEventQueue
  , newBoundedEventQueue
  , closeBoundedEventQueue
  , writeBoundedEventQueue
  , drainBoundedEventQueue

    -- * TriggerEvent transformer
  , BoundedTriggerT
  , runBoundedTriggerT
  ) where

import Control.Concurrent.STM
  ( STM
  , TBQueue
  , TMVar
  , atomically
  , flushTBQueue
  , newEmptyTMVarIO
  , newTBQueueIO
  , orElse
  , readTBQueue
  , readTMVar
  , tryPutTMVar
  , writeTBQueue
  )
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor (hoist))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Ref (MonadAtomicRef (..), MonadRef (..), Ref)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Coerce (coerce)
import Data.Dependent.Sum (DSum ((:=>)))
import Numeric.Natural (Natural)
import Reflex
import Reflex.Host.Class (MonadReflexCreateTrigger (..), newEventWithTriggerRef)

-- | A pending batch of trigger invocations. Each external @fire@ enqueues one
-- such batch (almost always a singleton).
type Batch t = [DSum (EventTriggerRef t) TriggerInvocation]

-- | A bounded, closeable queue of pending trigger invocations.
--
-- * Writes ('writeBoundedEventQueue') block when the queue is full
--   (backpressure to the producer).
-- * Once closed, writes return immediately without enqueueing, so producers
--   blocked at shutdown are released.
-- * Reads ('drainBoundedEventQueue') block until at least one batch is
--   available (or the queue is closed), then drain everything currently
--   queued.
data BoundedEventQueue t = BoundedEventQueue
  { _beqQueue :: !(TBQueue (Batch t))
  , _beqClosed :: !(TMVar ())
  -- ^ Full when the queue has been closed.
  }

-- | Create a bounded event queue with the given capacity. Capacity must be at
-- least 1; smaller values are clamped to 1.
newBoundedEventQueue :: Natural -> IO (BoundedEventQueue t)
newBoundedEventQueue cap =
  BoundedEventQueue
    <$> newTBQueueIO (max 1 cap)
    <*> newEmptyTMVarIO

-- | Close a queue. Idempotent. After this call, blocked and future writers
-- return immediately without enqueueing.
closeBoundedEventQueue :: BoundedEventQueue t -> IO ()
closeBoundedEventQueue q = atomically $ void $ tryPutTMVar (_beqClosed q) ()

-- | Enqueue a batch, blocking if the queue is full. Becomes a no-op once the
-- queue is closed (so a producer blocked here unblocks on shutdown).
writeBoundedEventQueue :: BoundedEventQueue t -> Batch t -> STM ()
writeBoundedEventQueue q batch =
  -- If there is room, write. Otherwise retry until there is room /or/ the
  -- queue is closed (in which case silently drop).
  writeTBQueue (_beqQueue q) batch `orElse` void (readTMVar (_beqClosed q))

-- | Block until at least one batch is available, then return it together with
-- every other batch currently queued, as a list of distinct batches (NOT
-- concatenated). The host fires each batch in its own frame so that multiple
-- occurrences of the same trigger that landed in the same drain are preserved
-- (Reflex collapses simultaneous same-trigger firings, so firing them as one
-- batch would drop occurrences). Returns 'Nothing' if the queue is closed and
-- empty.
drainBoundedEventQueue :: BoundedEventQueue t -> IO (Maybe [Batch t])
drainBoundedEventQueue q =
  atomically $
    let readFirst = Just <$> readTBQueue (_beqQueue q)
        waitClosed = Nothing <$ readTMVar (_beqClosed q)
    in do
         mFirst <- readFirst `orElse` waitClosed
         case mFirst of
           Nothing -> return Nothing
           Just first -> do
             rest <- flushTBQueue (_beqQueue q)
             return $ Just (first : rest)

----------------------------------------------------------------------------
-- TriggerEvent transformer
----------------------------------------------------------------------------

-- | A 'TriggerEvent' implementation that writes external triggers into a
-- 'BoundedEventQueue', applying backpressure when the queue is full. It is a
-- drop-in replacement for @TriggerEventT@ (same instance set); only its
-- 'TriggerEvent' instance differs, routing writes through
-- 'writeBoundedEventQueue'.
newtype BoundedTriggerT t m a = BoundedTriggerT
  { unBoundedTriggerT :: ReaderT (BoundedEventQueue t) m a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadFix
    , MonadIO
    , MonadMask
    , MonadThrow
    )

-- | Run a 'BoundedTriggerT' action against a particular 'BoundedEventQueue'.
runBoundedTriggerT :: BoundedTriggerT t m a -> BoundedEventQueue t -> m a
runBoundedTriggerT = runReaderT . unBoundedTriggerT

instance MonadTrans (BoundedTriggerT t) where
  lift = BoundedTriggerT . lift

instance MFunctor (BoundedTriggerT t) where
  hoist f = BoundedTriggerT . hoist f . unBoundedTriggerT

instance PrimMonad m => PrimMonad (BoundedTriggerT t m) where
  type PrimState (BoundedTriggerT t m) = PrimState m
  primitive = lift . primitive

instance PerformEvent t m => PerformEvent t (BoundedTriggerT t m) where
  type Performable (BoundedTriggerT t m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance PostBuild t m => PostBuild t (BoundedTriggerT t m) where
  getPostBuild = lift getPostBuild

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (BoundedTriggerT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance NotReady t m => NotReady t (BoundedTriggerT t m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady

instance MonadRef m => MonadRef (BoundedTriggerT t m) where
  type Ref (BoundedTriggerT t m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (BoundedTriggerT t m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadSample t m => MonadSample t (BoundedTriggerT t m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (BoundedTriggerT t m) where
  hold v0 v' = lift $ hold v0 v'
  holdDyn v0 v' = lift $ holdDyn v0 v'
  holdIncremental v0 v' = lift $ holdIncremental v0 v'
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE
  now = lift now

instance Adjustable t m => Adjustable t (BoundedTriggerT t m) where
  runWithReplace (BoundedTriggerT a0) a' = BoundedTriggerT $ runWithReplace a0 (coerceEvent a')
  traverseIntMapWithKeyWithAdjust f dm0 dm' =
    BoundedTriggerT $ traverseIntMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjust f dm0 dm' =
    BoundedTriggerT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' =
    BoundedTriggerT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'

instance
  ( Monad m
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadReflexCreateTrigger t m
  )
  => TriggerEvent t (BoundedTriggerT t m)
  where
  newTriggerEvent = do
    (e, t) <- newTriggerEventWithOnComplete
    return (e, \a -> t a $ return ())
  newTriggerEventWithOnComplete = do
    q <- BoundedTriggerT ask
    (eResult, reResultTrigger) <- lift newEventWithTriggerRef
    return . (,) eResult $ \a cb ->
      atomically $ writeBoundedEventQueue q [EventTriggerRef reResultTrigger :=> TriggerInvocation a cb]
  newEventWithLazyTriggerWithOnComplete f = do
    q <- BoundedTriggerT ask
    lift . newEventWithTrigger $ \t ->
      f $ \a cb -> do
        reResultTrigger <- newRef $ Just t
        atomically $ writeBoundedEventQueue q [EventTriggerRef reResultTrigger :=> TriggerInvocation a cb]
