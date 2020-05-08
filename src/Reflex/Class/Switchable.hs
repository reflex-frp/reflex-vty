{-|
Module: Reflex.Class.Switchable
Description: A class for things that can be switched on the firing of an event
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Class.Switchable where

import Control.Monad
import Reflex

-- | Class representing things that can be switched when the provided event occurs
class Reflex t => Switchable t w | w -> t where
  switching :: MonadHold t m => w -> Event t w -> m w

instance Reflex t => Switchable t (Event t a) where
  switching = switchHold

instance Reflex t => Switchable t (Dynamic t a) where
  switching a e = fmap join $ holdDyn a e

instance Reflex t => Switchable t (Behavior t a) where
  switching = switcher

instance (Reflex t, Switchable t a, Switchable t b) => Switchable t (a, b) where
  switching (a, b) e = (,)
    <$> switching a (fmap fst e)
    <*> switching b (fmap snd e)
