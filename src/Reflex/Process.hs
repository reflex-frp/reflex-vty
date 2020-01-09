{-|
Module: Reflex.Process
Description: Run interactive shell commands in reflex
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Reflex.Process
  ( createProcess
  , createRedirectedProcess
  , Process(..)
  , ProcessConfig(..)
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (mask_)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified GHC.IO.Handle as H
import GHC.IO.Handle (Handle)
import System.Exit (ExitCode)
import qualified System.Posix.Signals as P
import qualified System.Process as P
import System.Process hiding (createProcess)

import Reflex

-- | The inputs to a process
data ProcessConfig t = ProcessConfig
  { _processConfig_stdin :: Event t ByteString
  , _processConfig_signal :: Event t P.Signal
  }

-- | The output of a process
data Process t = Process
  { _process_handle :: P.ProcessHandle
  , _process_stdout :: Event t ByteString
  , _process_stderr :: Event t ByteString
  , _process_exit :: Event t ExitCode
  , _process_signal :: Event t P.Signal
  }

-- | Runs a process and uses the given input and output handler functions
-- to interact with the process.
--
-- NB: The 'std_in', 'std_out', and 'std_err' parameters of the
-- provided 'CreateProcess' are replaced with new pipes and all output is redirected
-- to those pipes.
createRedirectedProcess
  :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => (Handle -> IO (ByteString -> IO ()))
  -- ^ Builder for the stdin handler
  -> (Handle -> (ByteString -> IO ()) -> IO (IO ()))
  -- ^ Builder for the stdout and stderr handlers
  -> CreateProcess
  -> ProcessConfig t
  -> m (Process t)
createRedirectedProcess mkWriteInput mkReadOutput p (ProcessConfig input signal) = do
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
      sigOut <- performEvent $ ffor signal $ \sig -> liftIO $ do
        mpid <- P.getPid ph
        case mpid of
          Nothing -> return Nothing
          Just pid -> do
            P.signalProcess sig pid >> return (Just sig)
      let output h = do
            (e, trigger) <- newTriggerEvent
            reader <- liftIO $ mkReadOutput h trigger
            t <- liftIO $ forkIO reader
            return (e, t)
      (out, outThread) <- output hOut
      (err, errThread) <- output hErr
      (ecOut, ecTrigger) <- newTriggerEvent
      void $ liftIO $ forkIO $ waitForProcess ph >>= \ec -> mask_ $ do
        ecTrigger ec
        P.cleanupProcess po
        killThread outThread
        killThread errThread 
      return $ Process
        { _process_exit = ecOut
        , _process_stdout = out
        , _process_stderr = err
        , _process_signal = fmapMaybe id sigOut
        , _process_handle = ph
        }
    _ -> error "Reflex.Vty.Process.createRedirectedProcess: Created pipes were not returned by System.Process.createProcess."

-- | Run a shell process, feeding it input using an 'Event' and exposing its output
-- 'Event's representing the process exit code, stdout and stderr.
--
-- NB: The 'std_in', 'std_out', and 'std_err' parameters of the
-- provided 'CreateProcess' are replaced with new pipes and all output is redirected
-- to those pipes.
createProcess
  :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => CreateProcess
  -> ProcessConfig t
  -> m (Process t)
createProcess = createRedirectedProcess input output
  where
    input h = do
      H.hSetBuffering h H.NoBuffering
      let go b = do
            open <- H.hIsOpen h
            when open $ do
              writable <- H.hIsWritable h
              when writable $ Char8.hPutStrLn h b
      return go
    output h trigger = do
      H.hSetBuffering h H.LineBuffering
      let go = do
            open <- H.hIsOpen h
            readable <- H.hIsReadable h
            when (open && readable) $ do
              out <- BS.hGetSome h 32768
              if BS.null out
                then return ()
                else do
                  void $ trigger out
                  go
      return go
