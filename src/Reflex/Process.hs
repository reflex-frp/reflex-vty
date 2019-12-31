{-|
Module: Reflex.Process
Description: Run interactive shell commands in reflex
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Reflex.Process
  ( createProcess
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import GHC.IO.Handle (Handle, hClose)
import qualified System.IO.Streams as S
import qualified System.Process as P
import System.Process hiding (createProcess)

import Reflex

createRedirectedProcess
  :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => (Handle -> IO (ByteString -> IO ()))
  -> (Handle -> (ByteString -> IO ()) -> IO (IO ()))
  -> CreateProcess
  -> Event t ByteString
  -> m (Event t ByteString, Event t ByteString) 
createRedirectedProcess mkWriteInput mkReadOutput p input = do
  let redirectedProc = p
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  po@(mi, mout, merr, ph) <- liftIO $ P.createProcess redirectedProc 
  case (mi, mout, merr) of
    (Just hIn, Just hOut, Just hErr) -> do
      writeInput <- liftIO $ mkWriteInput hIn
      performEvent_ $ liftIO . writeInput <$> input
      let output h = do
            (e, trigger) <- newTriggerEvent
            reader <- liftIO $ mkReadOutput h trigger
            t <- liftIO $ forkIO reader
            return (e, t)
      (out, outThread) <- output hOut
      (err, errThread) <- output hErr
      void $ liftIO $ forkIO $ waitForProcess ph >> do
        P.cleanupProcess po
        killThread outThread
        killThread errThread 
      return (out, err)
    _ -> error "Reflex.Vty.Process.createProcess: Created pipes were not returned by System.Process.createProcess."

-- | Run a shell process, feeding it input using an 'Event' and exposing its output
-- as a pair of 'Event's representing stdout and stderr respectively.
--
-- NB: The 'std_in', 'std_out', and 'std_err' parameters of the
-- provided 'CreateProcess' are replaced with new pipes and all output is redirected
-- to those pipes.
createProcess
  :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => CreateProcess
  -> Event t ByteString
  -> m (Event t ByteString, Event t ByteString)
createProcess = createRedirectedProcess input output
  where
    input h = do
      s <- toOutputStreamWithLocking h
      return $ flip S.write s . Just
    output :: Handle -> (ByteString -> IO ()) -> IO (IO ())
    output h trigger = do
      s <- toInputStreamWithLocking h
      let go = S.read s >>= \case
            Nothing -> return ()
            Just x -> trigger x >> go
      return go
    toInputStreamWithLocking :: Handle -> IO (S.InputStream ByteString)
    toInputStreamWithLocking h = S.handleToInputStream h >>=
      S.atEndOfInput (hClose h) >>=
        S.lockingInputStream
    toOutputStreamWithLocking :: Handle -> IO (S.OutputStream ByteString)
    toOutputStreamWithLocking hin = S.handleToOutputStream hin >>=
      S.atEndOfOutput (hClose hin) >>=
        S.lockingOutputStream
