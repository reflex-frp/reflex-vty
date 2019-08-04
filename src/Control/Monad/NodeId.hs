{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.NodeId where

import Control.Monad.Reader
import Control.Monad.Ref
import Data.IORef

import Reflex
import Reflex.Host.Class

newtype NodeId = NodeId Integer
  deriving (Eq, Ord, Show)

class Monad m => MonadNodeId m where
  getNextNodeId :: m NodeId
  default getNextNodeId :: (MonadTrans t, MonadNodeId n, m ~ t n) => m NodeId
  getNextNodeId = lift getNextNodeId

newtype NodeIdT m a = NodeIdT { unNodeIdT :: ReaderT (IORef NodeId) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadHold t
    , MonadIO
    , MonadReflexCreateTrigger t
    , MonadSample t
    , MonadTrans
    , NotReady t
    , PerformEvent t
    , PostBuild t
    , TriggerEvent t
    , MonadRef
    )

instance MonadNodeId m => MonadNodeId (ReaderT x m)
instance MonadNodeId m => MonadNodeId (BehaviorWriterT t x m)
instance MonadNodeId m => MonadNodeId (DynamicWriterT t x m)
instance MonadNodeId m => MonadNodeId (EventWriterT t x m)
instance MonadNodeId m => MonadNodeId (TriggerEventT t m)
instance MonadNodeId m => MonadNodeId (PostBuildT t m)

instance Adjustable t m => Adjustable t (NodeIdT m) where
  runWithReplace (NodeIdT a) e = NodeIdT $ runWithReplace a $ fmap unNodeIdT e
  traverseIntMapWithKeyWithAdjust f m e = NodeIdT $ traverseIntMapWithKeyWithAdjust (\k v -> unNodeIdT $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = NodeIdT $ traverseDMapWithKeyWithAdjust (\k v -> unNodeIdT $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = NodeIdT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unNodeIdT $ f k v) m e

runNodeIdT :: MonadIO m => NodeIdT m a -> m a
runNodeIdT a = do
  ref <- liftIO $ newIORef $ NodeId 0
  runReaderT (unNodeIdT a) ref

instance MonadIO m => MonadNodeId (NodeIdT m) where
  getNextNodeId = NodeIdT $ do
    ref <- ask
    liftIO $ newNodeId ref

newNodeId :: IORef NodeId -> IO NodeId
newNodeId ref = atomicModifyIORef' ref $ \(NodeId n) -> (NodeId $ succ n, NodeId n)
