{-|
Module: Reflex.NotReady.Class.Orphans
Description: Orphan instances for NotReady
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.NotReady.Class.Orphans where

import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Reflex.NotReady.Class
import Reflex.BehaviorWriter.Base

instance (NotReady t m, Monoid w) => NotReady t (WriterT w m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady

instance (NotReady t m, Monoid w) => NotReady t (BehaviorWriterT t w m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady
