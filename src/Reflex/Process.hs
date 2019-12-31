{-|
Module: Reflex.Process
Description: Run interactive shell commands in reflex
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Reflex.Process
  ( createProcess
  , Process(..)
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified GHC.IO.Handle as H
import GHC.IO.Handle (Handle)
import System.Exit (ExitCode)
import qualified System.Process as P
import System.Process hiding (createProcess)

import Reflex

-- | The output of a process
data Process t = Process
  { _process_exit :: Event t ExitCode
  , _process_stdout :: Event t ByteString
  , _process_stderr :: Event t ByteString
  }

createRedirectedProcess
  :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => (Handle -> IO (ByteString -> IO ()))
  -> (Handle -> (ByteString -> IO ()) -> IO (IO ()))
  -> CreateProcess
  -> Event t ByteString
  -> m (Process t)
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
      (ecOut, ecTrigger) <- newTriggerEvent
      void $ liftIO $ forkIO $ waitForProcess ph >>= \ec -> do
        ecTrigger ec
        P.cleanupProcess po
        killThread outThread
        killThread errThread 
      return $ Process
        { _process_exit = ecOut
        , _process_stdout = out
        , _process_stderr = err
        }
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
  -> m (Process t)
createProcess = createRedirectedProcess input output
  where
    input h = return $ Char8.hPutStrLn h
    output h trigger = do
      let go = do
            open <- H.hIsOpen h
            readable <- H.hIsReadable h
            if open && readable
              then do
                out <- BS.hGet h 32768
                if BS.null out
                  then return ()
                  else do
                    void $ trigger out
                    go
              else go
      return go
