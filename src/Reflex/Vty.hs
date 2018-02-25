{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Vty where

import Reflex
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PerformEvent.Class
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Fix
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class
import Data.IORef (readIORef)
import Data.Dependent.Sum (DSum ((:=>)))
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode (NoBuffering))

import Data.Time
import qualified Graphics.Vty as V
import Control.Monad.Ref (Ref)
import Data.IORef (IORef)
import Control.Monad.Primitive (PrimMonad)

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
                  )
               => Event t (V.Event) -> {-PostBuildT t m-} PerformEventT t m (VtyResult t)

host
  :: (forall t m. VtyApp t m)
  -> IO ()
host vtyGuest = runSpiderHost $ do
  cfg <- liftIO V.standardIOConfig
  vty <- liftIO $ V.mkVty cfg

  (e, eTriggerRef) <- newEventWithTriggerRef
  -- (pb, pbTriggerRef) <- newEventWithTriggerRef
  (r, fireCommand) <- hostPerformEventT $ {- flip runPostBuildT pb $ -} do
    r <- vtyGuest e
    return r

  -- fireEventRef pbTriggerRef ()

  shutdown <- subscribeEvent $ _vtyResult_shutdown r

  fix $ \loop -> do
    vtyEvent <- liftIO $ V.nextEvent vty
    mETrigger <- liftIO $ readIORef eTriggerRef
    stop <- case mETrigger of
      Nothing -> return []
      Just eTrigger ->
        runFireCommand fireCommand [eTrigger :=> Identity vtyEvent] $ do
          readEvent shutdown >>= \case
            Nothing -> return False
            Just _ -> return True
    output <- runHostFrame $ sample $ _vtyResult_picture r
    liftIO $ V.update vty output
    case or stop of
      True -> liftIO $ V.shutdown vty
      False -> loop

guest :: VtyApp t m
guest e = do
  now <- performEvent $ fmap (\_ -> liftIO getCurrentTime) e
  -- now <- liftIO getCurrentTime
  -- ticks <- fmap show <$> tickLossy 1 now
  let shutdown = fforMaybe e $ \case
        V.EvKey V.KEsc _ -> Just ()
        _ -> Nothing
  picture <- hold V.emptyPicture $ V.picForImage . V.string mempty <$> leftmost [show <$> e, show <$> now]
  return $ VtyResult
    { _vtyResult_picture = picture
    , _vtyResult_refresh = never
    , _vtyResult_shutdown = shutdown
    }

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  host guest
