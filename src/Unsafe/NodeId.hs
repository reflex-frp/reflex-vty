module Unsafe.NodeId where

import Control.Monad.Primitive
import Data.IORef
import System.IO.Unsafe

newtype NodeId = NodeId Integer
  deriving (Eq, Ord, Show)

{-# NOINLINE nextNodeIdRef #-}
nextNodeIdRef :: IORef NodeId
nextNodeIdRef = unsafePerformIO $ newIORef $ NodeId 1

newNodeId :: IO NodeId
newNodeId = atomicModifyIORef' nextNodeIdRef $ \(NodeId n) -> (NodeId $ succ n, NodeId n)

{-# NOINLINE unsafeNodeId #-}
unsafeNodeId :: a -> NodeId
unsafeNodeId a = unsafePerformIO $ do
  touch a
  newNodeId
