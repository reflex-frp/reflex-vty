{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Vty where

import Reflex
import Reflex.Host.Class
import Control.Concurrent (forkIO)
import Control.Monad.Fix
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class
import Data.IORef (readIORef)
import Data.Dependent.Sum (DSum ((:=>)))
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode (NoBuffering))

import Data.Time
import qualified Graphics.Vty as V
import Control.Monad.Ref
import Data.IORef (IORef)
import Control.Monad.Primitive (PrimMonad)
import Control.Concurrent.Chan
import Control.Monad
import Data.Maybe

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

  someEvents <- liftIO newChan

  (r, fc@(FireCommand fire)) <- hostPerformEventT $ flip runPostBuildT pb $ flip runTriggerEventT someEvents $ do
    r <- vtyGuest e
    return r

  liftIO $ processAsyncEvents someEvents fc

  mPostBuildTrigger <- readRef pbTriggerRef
  forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
  vtyPostBuild <- sample $ _vtyResult_picture r
  liftIO $ V.update vty vtyPostBuild
    
  shutdown <- subscribeEvent $ _vtyResult_shutdown r

  fix $ \loop -> do
    vtyEvent <- liftIO $ V.nextEvent vty
    mETrigger <- liftIO $ readIORef eTriggerRef
    stop <- case mETrigger of
      Nothing -> return []
      Just eTrigger ->
        fire [eTrigger :=> Identity vtyEvent] $ do
          readEvent shutdown >>= \case
            Nothing -> return False
            Just _ -> return True
    output <- runHostFrame $ sample $ _vtyResult_picture r
    liftIO $ V.update vty output
    case or stop of
      True -> liftIO $ V.shutdown vty
      False -> loop

guest :: forall t m. VtyApp t m
guest e = do
  pb <- getPostBuild
  now <- liftIO getCurrentTime
  ticks <- fmap show <$> tickLossy 1 now
  let shutdown = fforMaybe e $ \case
        V.EvKey V.KEsc _ -> Just ()
        _ -> Nothing
  picture <- hold (V.picForImage $ V.string mempty "Initial") $ V.picForImage . V.string mempty <$> leftmost [show <$> e, show <$> ticks]
  return $ VtyResult
    { _vtyResult_picture = picture
    , _vtyResult_refresh = never
    , _vtyResult_shutdown = shutdown
    }

processAsyncEvents
  :: Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -> FireCommand t (SpiderHost Global)
  -> IO ()
processAsyncEvents events (FireCommand fire) = void $ forkIO $ forever $ do
  ers <- readChan events
  _ <- runSpiderHost $ do
    mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
      me <- readIORef er
      return $ fmap (\e -> e :=> Identity a) me
    _ <- fire (catMaybes mes) $ return ()
    liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
  return ()

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  host guest
