{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Description: PTY test driver for the 'mainWidget' entry point.
--
-- This executable runs @mainWidget@ applications in a PTY.
-- @test/pty_smoke.py@ controls the PTY.
-- The driver uses the real Vty instead of the test recorder.
-- It sends results and readiness messages through an inherited file descriptor.
-- @REFLEX_VTY_TEST_CONTROL_FD@ specifies this descriptor.
-- The driver does not display these messages in the UI.
--
-- Newline-delimited control protocol:
--
-- > READY
-- > VALUE <Haskell-show String>
-- > VIEWPORT <width> <height>
-- > SCROLL <first-visible-line> <visible-line-count>
-- > EXIT
--
-- Supported modes: @echo@, @pager@, @postbuild-exit@, @throw-after-start@.
module Main (main) where

import Control.Exception (ErrorCall (..), throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Default (def)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
  ( Behavior
  , Event
  , MonadSample (..)
  , PerformEvent
  , Performable
  , PostBuild
  , current
  , ffor
  , getPostBuild
  , leftmost
  , never
  , performEvent
  , performEvent_
  , sample
  , updated
  , zipDyn
  )
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (Handle, hFlush, hPutStrLn, hSetEncoding, stderr, utf8)
import System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd (..))
import Text.Read (readMaybe)

import Reflex.Vty
  ( HasColorProfile
  , HasCursor
  , HasDisplayRegion (..)
  , HasFocusReader
  , HasImageWriter
  , HasInput
  , HasScreenMode
  , HasTheme
  , MonadVtyApp
  , ScrollPos (..)
  , Scrollable (..)
  , ctrlc
  , displayHeight
  , displayWidth
  , key
  , mainWidget
  , mouseScroll
  , scrollable
  , text
  , textInput
  , _scrollable_scrollPosition
  , _textInput_value
  )

-- | The total line count of the pager content.
-- Lines are short enough to prevent wrapping at all tested terminal widths.
-- The Python driver uses the same total line count.
pagerTotalLines :: Int
pagerTotalLines = 40

main :: IO ()
main = do
  mode <-
    getArgs >>= \case
      [m] | m `elem` modes -> pure m
      _ -> do
        hPutStrLn
          stderr
          ( "usage: reflex-vty-test-driver <mode>  (modes: "
              ++ unwords modes
              ++ "); control fd taken from REFLEX_VTY_TEST_CONTROL_FD"
          )
        exitFailure
  ctl <- openControlFd
  mainWidget def (driverWidget ctl mode)
  where
    modes = ["echo", "pager", "postbuild-exit", "throw-after-start"]

-- | Convert the inherited control file descriptor to a 'Handle'.
openControlFd :: IO Handle
openControlFd = do
  mf <- lookupEnv "REFLEX_VTY_TEST_CONTROL_FD"
  fd <- case mf >>= readMaybe of
    Nothing -> failWithoutFd
    Just fd -> pure (Fd fd)
  h <- fdToHandle fd
  hSetEncoding h utf8
  pure h
  where
    failWithoutFd = do
      hPutStrLn stderr "REFLEX_VTY_TEST_CONTROL_FD must name an inherited file descriptor"
      exitFailure

-- | Write one protocol line to the control channel and flush it.
ctlLine :: Handle -> String -> IO ()
ctlLine h msg = do
  hPutStrLn h msg
  hFlush h

-- | The widget run by 'mainWidget'. Reports READY and the root viewport on
-- the control channel, delegates mode-specific behaviour, and reports EXIT
-- on whichever shutdown event the mode produces.
driverWidget
  :: forall t m
   . ( MonadVtyApp t m
     , MonadIO m
     , HasImageWriter t m
     , HasDisplayRegion t m
     , HasFocusReader t m
     , HasInput t m
     , HasTheme t m
     , HasColorProfile t m
     , HasCursor t m
     , HasScreenMode t m
     )
  => Handle
  -> String
  -> m (Event t ())
driverWidget ctl mode = do
  pb <- getPostBuild
  ready <- performEvent $ ffor pb $ \_ -> liftIO (ctlLine ctl "READY")

  size <- zipDyn <$> displayWidth <*> displayHeight
  performEvent_ $ ffor (leftmost [() <$ updated size, pb]) $ \_ -> do
    (w, h') <- sample (current size)
    liftIO (ctlLine ctl ("VIEWPORT " ++ show w ++ " " ++ show h'))

  shutdown <- case mode of
    "echo" -> echoMode ctl
    "pager" -> pagerMode ctl pb
    "postbuild-exit" -> pure pb
    _ -> throwAfterStartMode ready

  performEvent_ $ ffor shutdown $ \_ -> liftIO (ctlLine ctl "EXIT")
  pure shutdown

-- | Report each change in the text input value as @VALUE <Haskell-show String>@.
-- Ctrl-C stops the widget.
echoMode
  :: forall t m
   . ( MonadVtyApp t m
     , MonadIO m
     , HasImageWriter t m
     , HasDisplayRegion t m
     , HasFocusReader t m
     , HasInput t m
     , HasTheme t m
     )
  => Handle
  -> m (Event t ())
echoMode ctl = do
  ti <- textInput def
  performEvent_ $ ffor (updated (_textInput_value ti)) $ \t ->
    liftIO (ctlLine ctl ("VALUE " ++ show (T.unpack t)))
  quit <- ctrlc
  pure (void quit)

-- | A scrollable text widget.
-- Arrow keys, mouse wheel events, and viewport changes produce @SCROLL <first-visible-line> <visible-line-count>@ reports.
-- Ctrl-C stops the widget.
pagerMode
  :: forall t m
   . ( MonadVtyApp t m
     , MonadIO m
     , HasImageWriter t m
     , HasDisplayRegion t m
     , HasFocusReader t m
     , HasInput t m
     , HasTheme t m
     )
  => Handle
  -> Event t ()
  -> m (Event t ())
pagerMode ctl pb = do
  let content :: T.Text
      content = T.intercalate "\n" (T.pack . pad3 <$> [1 .. pagerTotalLines])
      pad3 :: Int -> String
      pad3 n = let s = show n in replicate (3 - length s) '0' ++ s

  sc <- fmap fst $
    scrollable def $ do
      text (pure content)
      pure (never, ())

  kup <- key V.KUp
  kdown <- key V.KDown
  wheel <- mouseScroll
  dh <- displayHeight
  let reportTriggers :: Event t ()
      reportTriggers =
        leftmost
          [ void kup
          , void kdown
          , void wheel
          , pb
          , () <$ updated dh
          ]
      scrollPos :: Behavior t ScrollPos
      scrollPos = _scrollable_scrollPosition sc
      viewportHeight' :: Behavior t Int
      viewportHeight' = current dh

  performEvent_ $ ffor reportTriggers $ \_ -> do
    pos <- sample scrollPos
    h' <- sample viewportHeight'
    liftIO $
      ctlLine ctl $
        "SCROLL "
          ++ show (firstVisibleLine pos pagerTotalLines h')
          ++ " "
          ++ show (min pagerTotalLines h')
  quit <- ctrlc
  pure (void quit)

-- | Calculate the first visible line from the scroll position, total line count, and viewport height.
firstVisibleLine :: ScrollPos -> Int -> Int -> Int
firstVisibleLine pos total h = case pos of
  ScrollPos_Top -> 0
  ScrollPos_Line n -> n
  ScrollPos_Bottom -> max 0 (total - h)

-- | Raise an exception after the driver sends READY.
-- The host must restore the terminal and stop with a nonzero exit code.
throwAfterStartMode :: forall t m. (PostBuild t m, PerformEvent t m, MonadIO (Performable m)) => Event t () -> m (Event t ())
throwAfterStartMode pb = do
  performEvent_ $ ffor pb $ \_ ->
    liftIO $ throwIO (ErrorCall "reflex-vty-test-driver: requested failure (throw-after-start)")
  pure never
