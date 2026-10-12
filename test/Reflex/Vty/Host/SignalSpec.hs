{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description: OS signal installation, mapping, and restoration.
--
-- Signal handlers are process-global state. The delivery scenarios
-- therefore run in isolated child processes of this test executable.
-- @test/Spec.hs@ dispatches the children via @REFLEX_VTY_SIGNAL_CHILD@
-- before Hspec starts. The parent only spawns children, raises
-- signals at them, and checks the reported sequence; it never
-- installs handlers itself. POSIX children verify real signal
-- delivery, mapping, and restoration. Windows children exercise
-- console-handler mapping and restoration. They have no desktop
-- console and no real OS shutdown, so they prove installation,
-- mapping, and restoration, not OS delivery.
module Reflex.Vty.Host.SignalSpec
  ( spec
  , runSignalChild
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (waitCatch)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracketOnError, catch)
import Control.Monad (forM_, forever)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO
import System.Process
import System.Timeout (timeout)
import Test.Hspec

#if defined(mingw32_HOST_OS)
import qualified GHC.ConsoleHandler as Console
#else
import System.Posix.Signals
  ( Handler (Catch)
  , Signal
  , installHandler
  , raiseSignal
  , sigHUP
  , sigINT
  , sigTERM
  , signalProcess
  )
#endif

import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import Reflex

import Reflex.Vty.Host (AppSignal (..), VtyApp, VtyResult (..), defaultVtyAppConfig)
import Reflex.Vty.Host.Signal ()
import Reflex.Vty.Test.AppHarness (defaultRecordingVtyConfig, recordingVty, runHostAsync)

-- | Deadline for one expected line from a child (microseconds).
lineDeadline :: Int
lineDeadline = 10 * 1000000

-- | Background reader that turns a handle into a line mailbox.
startLineReader :: Handle -> IO (MVar (Maybe BS.ByteString))
startLineReader h = do
  v <- newEmptyMVar
  _ <-
    forkIO $
      (forever (BS.hGetLine h >>= putMVar v . Just))
        `catch` \(_ :: SomeException) -> putMVar v Nothing
  pure v

-- | Read the next line with a deadline. A deadline expiry and the
-- end of output are both reported as 'Nothing'.
nextLine :: MVar (Maybe BS.ByteString) -> IO (Maybe String)
nextLine v = do
  r <- timeout lineDeadline (takeMVar v)
  pure $ case r of
    Nothing -> Nothing
    Just Nothing -> Nothing
    Just (Just bs) -> Just (BS.unpack bs)

-- | Read lines until one equals the expected string.
awaitLine :: MVar (Maybe BS.ByteString) -> String -> IO ()
awaitLine v expected = do
  ml <- nextLine v
  case ml of
    Nothing -> expectationFailure ("child closed stdout while waiting for " ++ show expected) >> pure ()
    Just l
      | l == expected -> pure ()
      | otherwise -> awaitLine v expected

-- | Read lines until one starts with the prefix and return it.
awaitPrefix :: MVar (Maybe BS.ByteString) -> String -> IO String
awaitPrefix v prefix = do
  ml <- nextLine v
  case ml of
    Nothing -> expectationFailure ("child closed stdout while waiting for prefix " ++ show prefix) >> pure ""
    Just l
      | prefix `isPrefixOf` l -> pure l
      | otherwise -> awaitPrefix v prefix

-- | The child modes exercised on this platform.
childModes :: [(String, String, String)]
#if defined(mingw32_HOST_OS)
childModes = [("console", "", "")]
#else
childModes =
  [ ("int", "AppSignal_Interrupt", "sigINT")
  , ("term", "AppSignal_Terminate", "sigTERM")
  , ("hup", "AppSignal_Hangup", "sigHUP")
  ]
#endif

spec :: Spec
spec = describe "Reflex.Vty.Host.Signal" $ do
  forM_ childModes $ \(mode, mapped, _signalName) ->
    it ("installs, maps, and restores handlers for " ++ mode ++ " in an isolated child") $
      runChildScenario mode mapped

-- | Spawn this executable as a signal child, drive the expected
-- sequence, and verify clean termination.
runChildScenario :: String -> String -> IO ()
runChildScenario mode mapped = do
  exe <- getExecutablePath
  parentEnv <- getEnvironment
  tixOverride <- coverageChildTix mode
  let childEnv = (tixOverride ++) . (("REFLEX_VTY_SIGNAL_CHILD", mode) :) $ parentEnv
  bracketOnError
    -- The child gets its own process group. No signal in these
    -- scenarios can then reach the pane or session that runs the
    -- suite.
    ( createProcess
        (proc exe []) {env = Just childEnv, std_out = CreatePipe, create_group = True}
    )
    (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
    $ \(mIn, mOut, _, ph) -> do
      case mOut of
        Nothing -> expectationFailure "child stdout was not piped"
        Just hOut -> do
          hSetBinaryMode hOut False
          hSetBuffering hOut LineBuffering
          v <- startLineReader hOut
          awaitLine v "READY"
          checkSignalDelivery ph v mode mapped
          ec <- waitForProcess ph
          ec `shouldBe` ExitSuccess

checkSignalDelivery :: ProcessHandle -> MVar (Maybe BS.ByteString) -> String -> String -> IO ()
#if !defined(mingw32_HOST_OS)
checkSignalDelivery ph v mode mapped = do
  mpid <- getPid ph
  case mpid of
    Nothing -> expectationFailure "could not get child pid"
    Just pid -> do
      signalProcess (childSignal mode) pid
      appLine <- awaitPrefix v "APP "
      appLine `shouldBe` ("APP " ++ mapped)
      awaitLine v "HOSTDONE"
      -- After host shutdown, delivery must reach the prior handler.
      signalProcess (childSignal mode) pid
      awaitLine v "PRIOR"
#else
checkSignalDelivery _ v _ _ = do
  forM_ consoleEvents $ \_ -> do
    l <- awaitPrefix v "APP "
    take 4 l `shouldBe` "APP "
  awaitLine v "COUNT 1"
  awaitLine v "HOSTDONE"
#endif

#if defined(mingw32_HOST_OS)
consoleEvents :: [()]
consoleEvents = [(), (), (), (), ()]
#else
childSignal :: String -> Signal
childSignal mode = case mode of
  "int" -> sigINT
  "hup" -> sigHUP
  _ -> sigTERM
#endif

-- | The tix-file override for coverage runs: each child gets its own
-- file so children never overwrite the parent's tix.
coverageChildTix :: String -> IO [(String, String)]
coverageChildTix mode = do
  mdir <- lookupEnv "REFLEX_VTY_COVERAGE_CHILD_DIR"
  pure $ case mdir of
    Nothing -> []
    Just d -> [("HPCTIXFILE", d </> ("signal-" ++ mode ++ ".tix"))]

----------------------------------------------------------------------------
-- Child process implementation
----------------------------------------------------------------------------

-- | Entry point for child modes, dispatched from test/Spec.hs before
-- Hspec starts.
runSignalChild :: IO ()
runSignalChild = do
  hSetBuffering stdout LineBuffering
  mmode <- lookupEnv "REFLEX_VTY_SIGNAL_CHILD"
  case mmode of
    Nothing -> putStrLn "CHILDERR no mode" >> exitFailure
#if defined(mingw32_HOST_OS)
    Just _ -> windowsConsoleChild
#else
    Just mode -> posixSignalChild mode
#endif

#if !defined(mingw32_HOST_OS)

posixSignalChild :: String -> IO ()
posixSignalChild mode = do
  let sig = childSignal mode
  priorDone <- newEmptyMVar
  _ <- installHandler sig (Catch (putStrLn "PRIOR" >> putMVar priorDone ())) Nothing
  (vty, _st) <- recordingVty defaultRecordingVtyConfig
  hostA <- runHostAsync defaultVtyAppConfig vty signalGuest
  r <- waitCatch hostA
  case r of
    Left e -> putStrLn ("HOSTFAIL " ++ show e) >> exitFailure
    Right () -> pure ()
  putStrLn "HOSTDONE"
  raiseSignal sig
  -- The restored prior handler signals completion from its own
  -- thread.
  done <- timeout (5 * 1000000) (takeMVar priorDone)
  case done of
    Nothing -> putStrLn "CHILDERR prior handler never ran" >> exitFailure
    Just () -> exitSuccess

-- | The guest reports readiness at post-build. The host installs
-- signal handlers before post-build. The guest reports each received
-- application signal and shuts down on the first one.
signalGuest :: VtyApp t m
signalGuest _dr _inp sigs = do
  pb <- getPostBuild
  performEvent_ $ ffor pb $ \_ -> liftIO (putStrLn "READY")
  performEvent_ $ ffor sigs $ \s -> liftIO (putStrLn ("APP " ++ show s))
  return
    VtyResult
      { _vtyResult_picture = constant (V.picForImage V.emptyImage)
      , _vtyResult_shutdown = () <$ sigs
      }

#else

-- | Windows child scenario. Install a counting handler. Install the
-- library's handler. Retrieve and invoke its callback for each
-- console event to observe the mapping. Restore all handlers. Verify
-- that the counting handler is active again.
windowsConsoleChild :: IO ()
windowsConsoleChild = do
  countRef <- newIORef (0 :: Int)
  _ <- Console.installHandler (Console.Catch (\_ -> modifyIORef' countRef (+1)))
  restore <- installAppSignalHandlers $ \s ->
    putStrLn ("APP " ++ show s)
  -- Temporarily retrieve the active (library) handler; invoking its
  -- callback with each console event exercises the mapping without a
  -- desktop console or a real shutdown.
  active <- Console.installHandler (Console.Catch (pure ()))
  case active of
    Console.Catch act ->
      forM_ [minBound .. maxBound] act
    _ -> pure ()
  _ <- Console.installHandler active -- restore the retrieved handler
  restore -- restore the counting handler
  -- The counting handler is active again.
  counting <- Console.installHandler (Console.Catch (pure ()))
  case counting of
    Console.Catch act -> act Console.ControlC
    _ -> pure ()
  _ <- Console.installHandler counting
  n <- readIORef countRef
  putStrLn ("COUNT " ++ show n)
  putStrLn "HOSTDONE"

#endif
