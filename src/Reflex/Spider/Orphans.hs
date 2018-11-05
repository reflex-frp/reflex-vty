{-|
Module: Reflex.Spider.Orphans
Description: Orphan instances for SpiderTimeline and SpiderHost
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.Spider.Orphans where

import Reflex
import Reflex.NotReady.Class
import Reflex.Spider.Internal

-- TODO move this to reflex
instance NotReady (SpiderTimeline x) (SpiderHost x) where
  notReadyUntil _ = pure ()
  notReady = pure ()

instance HasSpiderTimeline x => NotReady (SpiderTimeline x) (PerformEventT (SpiderTimeline x) (SpiderHost x)) where
  notReadyUntil _ = pure ()
  notReady = pure ()
