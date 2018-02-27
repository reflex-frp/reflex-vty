{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Vty where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad (forM, forM_, forever)
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef, Ref, readRef)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (IORef)
import Data.IORef (readIORef)
import Data.Maybe (catMaybes)

import Reflex
import Reflex.Host.Class
import qualified Graphics.Vty as V

data VtyResult t = VtyResult
  { _vtyResult_picture :: Behavior t V.Picture
  , _vtyResult_refresh :: Event t ()
  , _vtyResult_shutdown :: Event t ()
  }

type VtyApp t m = ( Reflex t
                  , MonadHold t m
                  , MonadFix m
                  , PrimMonad (HostFrame t)
                  , ReflexHost t
                  , MonadIO (HostFrame t)
                  , Ref m ~ IORef
                  , Ref (HostFrame t) ~ IORef
                  , MonadRef (HostFrame t)
                  )
               => Event t (V.Event)
               -> TriggerEventT t (PostBuildT t (PerformEventT t m)) (VtyResult t)

host
  :: (forall t m. VtyApp t m)
  -> IO ()
host vtyGuest = runSpiderHost $ do
  cfg <- liftIO V.standardIOConfig
  vty <- liftIO $ V.mkVty cfg

  (e, eTriggerRef) <- newEventWithTriggerRef
  (pb, pbTriggerRef) <- newEventWithTriggerRef

  events <- liftIO newChan

  (r, fc@(FireCommand fire)) <- hostPerformEventT $
    flip runPostBuildT pb $
      flip runTriggerEventT events $
        vtyGuest e

  let updateVty = sample (_vtyResult_picture r) >>= liftIO . V.update vty

  mPostBuildTrigger <- readRef pbTriggerRef
  forM_ mPostBuildTrigger $ \postBuildTrigger ->
    fire [postBuildTrigger :=> Identity ()] $ return ()
  updateVty

  shutdown <- subscribeEvent $ _vtyResult_shutdown r

  nextEventThread <- liftIO $ forkIO $ forever $ do
    ne <- V.nextEvent vty
    writeChan events $ [EventTriggerRef eTriggerRef :=> TriggerInvocation ne (return ())]

  fix $ \loop -> do
    ers <- liftIO $ readChan events
    stop <- do
      fireEventTriggerRefs fc ers $ do
        readEvent shutdown >>= \case
          Nothing -> return False
          Just _ -> return True
    if or stop
      then liftIO $ do
        killThread nextEventThread
        V.shutdown vty
      else do
        updateVty
        loop

-- TODO Some part of this is probably general enough to belong in reflex
fireEventTriggerRefs
  :: (Monad (ReadPhase m), MonadIO m)
  => FireCommand t m
  -> [DSum (EventTriggerRef t) TriggerInvocation]
  -> ReadPhase m a
  -> m [a]
fireEventTriggerRefs (FireCommand fire) ers rcb = do
  mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
    me <- readIORef er
    return $ fmap (\e -> e :=> Identity a) me
  let es = catMaybes mes
  a <- fire es rcb
  liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
  return a
