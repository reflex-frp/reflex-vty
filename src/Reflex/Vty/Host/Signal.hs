{-# LANGUAGE CPP #-}

-- |
-- Module: Reflex.Vty.Host.Signal
-- Description: Cross-platform OS signal handling for reflex-vty.
module Reflex.Vty.Host.Signal
  ( AppSignal (..)
  , installAppSignalHandlers
  ) where

import Foreign.C.Types (CInt)

#if defined(mingw32_HOST_OS)
import Control.Monad (void)
import qualified GHC.ConsoleHandler as Console
#else
import Control.Monad (forM_)
import System.Posix.Signals (Handler (Catch), installHandler, sigHUP, sigINT, sigTERM)
#endif

-- | An OS-level signal delivered to a running application.
--
--   * 'AppSignal_Interrupt': POSIX @SIGINT@ or Windows @ControlC@
--   * 'AppSignal_Terminate': POSIX @SIGTERM@ or Windows @Shutdown@
--   * 'AppSignal_Hangup': POSIX @SIGHUP@ or Windows @Close@ or Windows @Logoff@
--
-- All other signals can be shoved into the 'AppSignal_Platform' escape hatch.
data AppSignal
  = AppSignal_Interrupt
  | AppSignal_Terminate
  | AppSignal_Hangup
  | AppSignal_Platform CInt
  deriving (Eq, Show)

-- | Install handlers for OS signals
installAppSignalHandlers :: (AppSignal -> IO ()) -> IO ()

#if defined(mingw32_HOST_OS)

-- via 'GHC.ConsoleHandler'
installAppSignalHandlers fire =
  void $ Console.installHandler $ Console.Catch (fire . toAppSignal)
  where
    toAppSignal :: Console.ConsoleEvent -> AppSignal
    toAppSignal ev = case ev of
      Console.ControlC -> AppSignal_Interrupt -- Ctrl-C: same intent as SIGINT.
      Console.Break -> AppSignal_Interrupt -- Ctrl-Break: closest to an interrupt.
      Console.Close -> AppSignal_Hangup -- console window closed: terminal going away.
      Console.Logoff -> AppSignal_Hangup -- user logging off: session going away.
      Console.Shutdown -> AppSignal_Terminate -- system shutting down: clean termination.

#else

installAppSignalHandlers fire =
  forM_ signals $ \(sig, appSignal) ->
    installHandler sig (Catch (fire appSignal)) Nothing
  where
    signals =
      [ (sigINT, AppSignal_Interrupt)
      , (sigTERM, AppSignal_Terminate)
      , (sigHUP, AppSignal_Hangup)
      ]

#endif
