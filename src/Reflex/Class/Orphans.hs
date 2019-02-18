{-|
Module: Reflex.Class.Orphans
Description: Orphan instances for Dynamic. These should be upstreamed.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.Class.Orphans where

import Control.Applicative
import Reflex.Class

instance (Num a, Reflex t) => Num (Dynamic t a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate
  (-) = liftA2 (-)
