{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Reflex.Vty.Host.Signal
-- Description: Cross-platform OS signal handling for reflex-vty.
module Reflex.Vty.Host.Signal
  ( AppSignal (..)
  , installAppSignalHandlers
  ) where

import Control.Exception (SomeException, mask, throwIO, try)
import Control.Monad (forM_, void)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Foreign.C.Types (CInt)

#if defined(mingw32_HOST_OS)
import qualified GHC.ConsoleHandler as Console
#else
import System.Posix.Signals (Handler (Catch), installHandler, sigHUP, sigINT, sigTERM)
#endif

-- | An OS-level signal delivered to a running application.
--
--   * 'AppSignal_Interrupt': POSIX @SIGINT@ or Windows @ControlC@
--   * 'AppSignal_Terminate': POSIX @SIGTERM@ or Windows @Shutdown@
--   * 'AppSignal_Hangup': POSIX @SIGHUP@ or Windows @Close@ or Windows @Logoff@
--
-- All other signals go through the 'AppSignal_Platform' constructor.
data AppSignal
  = AppSignal_Interrupt
  | AppSignal_Terminate
  | AppSignal_Hangup
  | AppSignal_Platform CInt
  deriving (Eq, Show)

-- | Install handlers for OS signals. Returns an action that restores
-- the previous handlers. Acquisition is masked. If any registration
-- fails, every handler installed so far is restored before the
-- exception propagates.
installAppSignalHandlers :: (AppSignal -> IO ()) -> IO (IO ())

#if defined(mingw32_HOST_OS)

-- via 'GHC.ConsoleHandler'
installAppSignalHandlers fire =
  mask $ \_ -> do
    r <- try (Console.installHandler (Console.Catch (fire . toAppSignal)))
    case r of
      Left (e :: SomeException) -> throwIO e
      Right previous -> pure (void (Console.installHandler previous))
  where
    toAppSignal :: Console.ConsoleEvent -> AppSignal
    toAppSignal ev = case ev of
      Console.ControlC -> AppSignal_Interrupt -- Ctrl-C: same intent as SIGINT.
      Console.Break -> AppSignal_Interrupt -- Ctrl-Break: closest to an interrupt.
      Console.Close -> AppSignal_Hangup -- Console window closed: the terminal is going away.
      Console.Logoff -> AppSignal_Hangup -- User logging off: the session is going away.
      Console.Shutdown -> AppSignal_Terminate -- System shutting down: clean termination.

#else

installAppSignalHandlers fire =
  mask $ \_ -> do
    installed <- newIORef ([] :: [(Handler, Handler -> IO ())])
    r <-
      try $
        forM_ signals $ \(sig, appSignal) -> do
          previous <- installHandler sig (Catch (fire appSignal)) Nothing
          modifyIORef' installed ((previous, \h -> void (installHandler sig h Nothing)) :)
    case r of
      Left (e :: SomeException) -> do
        rollback installed
        throwIO e
      Right () -> pure (rollback installed)
  where
    rollback :: IORef [(Handler, Handler -> IO ())] -> IO ()
    rollback installed =
      readIORef installed >>= \ps -> forM_ ps (\(previous, restoreOne) -> restoreOne previous)
    signals =
      [ (sigINT, AppSignal_Interrupt)
      , (sigTERM, AppSignal_Terminate)
      , (sigHUP, AppSignal_Hangup)
      ]

#endif
