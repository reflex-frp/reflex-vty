{-|
Module: Reflex.Spider.Orphans
Description: Orphan instances for SpiderTimeline and SpiderHost
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.Spider.Orphans where

#if MIN_VERSION_reflex(0,6,3)
#else
import Reflex
import Reflex.Spider.Internal

instance NotReady (SpiderTimeline x) (SpiderHost x) where
  notReadyUntil _ = pure ()
  notReady = pure ()

instance HasSpiderTimeline x => NotReady (SpiderTimeline x) (PerformEventT (SpiderTimeline x) (SpiderHost x)) where
  notReadyUntil _ = pure ()
  notReady = pure ()
#endif
