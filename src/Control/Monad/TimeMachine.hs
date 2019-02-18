{-|
Module: Control.Monad.TimeMachine
Description: A bi-directional state monad transformer
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.TimeMachine where

import Control.Monad.Fix
import Control.Monad.Reader
import Reflex
import Reflex.NotReady.Class

-- | A bi-directional state monad transformer. State changes may be propagated backward ("to the past")
-- or forward ("to the future"). Backward and forward correspond to the order in which monadic actions
-- are run.
-- The 3-tuple that is returned by 'runTimeMachineT' is made up of:
-- * "back": The value sent by the *first* 'sendToPast'
-- * "fwd": The value sent by the *last* 'sendToFuture'
-- * "a": The result
newtype TimeMachineT back fwd m a = TimeMachineT { runTimeMachineT :: back -> fwd -> m (back, fwd, a) }
  deriving (Functor)

instance MonadFix m => Applicative (TimeMachineT back fwd m) where
  (<*>) = ap
  pure = return

instance MonadFix m => Monad (TimeMachineT back fwd m) where
  x >>= f = TimeMachineT $ \back fwd -> do
    rec (back'', fwd', a) <- runTimeMachineT x back' fwd
        (back', fwd'', b) <- runTimeMachineT (f a) back fwd'
    return (back'', fwd'', b)
  return v = TimeMachineT $ \back fwd -> return (back, fwd, v)

instance MonadFix m => MonadFix (TimeMachineT back fwd m) where
  mfix f = TimeMachineT $ \back fwd -> do
    rec (back', fwd', a) <- runTimeMachineT (f a) back fwd
    return (back', fwd', a)

instance MonadTrans (TimeMachineT back fwd) where
  lift x = TimeMachineT $ \back fwd -> (back, fwd,) <$> x

instance (MonadFix m, MonadReader r m) => MonadReader r (TimeMachineT back fwd m) where
  ask = lift ask
  local f x = TimeMachineT $ \back fwd -> local f $ runTimeMachineT x back fwd

instance (MonadFix m, MonadSample t m) => MonadSample t (TimeMachineT back fwd m) where
  sample = lift . sample

instance (MonadFix m, MonadHold t m) => MonadHold t (TimeMachineT back fwd m) where
  buildDynamic a b = lift $ buildDynamic a b
  headE = lift . headE

instance (MonadFix m, NotReady t m) => NotReady t (TimeMachineT back fwd m)

instance (MonadFix m, EventWriter t w m) => EventWriter t w (TimeMachineT back fwd m) where
  tellEvent = lift . tellEvent

instance (MonadFix m, MonadBehaviorWriter t w m) => MonadBehaviorWriter t w (TimeMachineT back fwd m) where
  tellBehavior = lift . tellBehavior

instance (MonadFix m, MonadDynamicWriter t w m) => MonadDynamicWriter t w (TimeMachineT back fwd m) where
  tellDyn = lift . tellDyn

-- | Send a message to the preceding 'recvFromFuture'.
sendToPast :: Monad m => back -> TimeMachineT back fwd m ()
sendToPast x = TimeMachineT $ \_ fwd -> return (x, fwd, ())

-- | Receive a message from the next 'sendToPast'. If it does not exist,
-- the value received will be the one passed to 'runTimeMachineT'.
recvFromFuture :: Monad m => TimeMachineT back fwd m back
recvFromFuture = TimeMachineT $ \back fwd -> return (back, fwd, back)

-- | Send a message to the next 'recvFromPast'.
sendToFuture :: Monad m => fwd -> TimeMachineT back fwd m ()
sendToFuture x = TimeMachineT $ \back _ -> return (back, x, ())

-- | Receive a message from the preceding 'sendToFuture'. If it does not exist,
-- the value received will be the one passed to 'runTimeMachineT'.
recvFromPast :: Monad m => TimeMachineT back fwd m fwd
recvFromPast = TimeMachineT $ \back fwd -> return (back, fwd, fwd)
