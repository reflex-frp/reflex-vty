{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Reflex.Vty where

import Reflex
import Reflex.Host.Class
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Fix
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Dependent.Sum (DSum ((:=>)))
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode (NoBuffering))

import qualified Graphics.Vty as V

data VtyResult t = VtyResult
  { _vtyResult_picture :: Behavior t V.Picture
  , _vtyResult_refresh :: Event t ()
  , _vtyResult_shutdown :: Event t ()
  }

type VtyApp t m = (Reflex t, MonadHold t m, MonadFix m) => Event t (V.Event) -> m (VtyResult t)

host
  :: (forall t m. VtyApp t m)
  -> IO ()
host vtyGuest = runSpiderHost $ do
  cfg <- liftIO V.standardIOConfig
  vty <- liftIO $ V.mkVty cfg

  (e, eTriggerRef) <- newEventWithTriggerRef
  r <- runHostFrame $ vtyGuest e
  shutdown <- subscribeEvent $ _vtyResult_shutdown r

  fix $ \loop -> do
    vtyEvent <- liftIO $ V.nextEvent vty
    mETrigger <- liftIO $ readIORef eTriggerRef
    next <- case mETrigger of
      Nothing -> return loop
      Just eTrigger ->
        fireEventsAndRead [eTrigger :=> Identity vtyEvent] $ do
          readEvent shutdown >>= \case
            Nothing -> return loop
            Just _ -> return $ liftIO $ V.shutdown vty
    output <- runHostFrame $ sample $ _vtyResult_picture r
    liftIO $ V.update vty output
    next

guest :: VtyApp t m
guest e = do
  let shutdown = fforMaybe e $ \case
        V.EvKey V.KEsc _ -> Just ()
        _ -> Nothing
  picture <- hold V.emptyPicture $ V.picForImage . V.string mempty . show <$> e
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
