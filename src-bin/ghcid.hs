{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex.Process
import Reflex.Vty

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as List
import Data.Sequence (Seq)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Graphics.Vty as V
import Text.Regex.TDFA as Regex
import Text.Regex.TDFA.ByteString ()
import qualified System.Directory as Dir
import qualified System.FilePath.Posix as FS
import qualified System.FSNotify as FS
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Posix.Signals as P
import qualified System.Process as P

watchDirectory
  :: (Reflex t, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => FS.WatchConfig
  -> Event t FilePath
  -> m (Event t FS.Event)
watchDirectory cfg path =
  performEventAsync $ ffor path $ \p cb -> liftIO $ void $ forkIO $
    FS.withManagerConf cfg $ \mgr -> do
      _ <- FS.watchTree mgr p (const True) cb
      forever $ threadDelay 1000000

prompt :: IsString a => a
prompt = "~WAITING~"

ghcid
  :: (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m), PostBuild t m, MonadIO m, MonadFix m, MonadHold t m)
  => FilePath
  -> m (Process t, Event t (Seq FS.Event), Dynamic t LoadState, Event t ())
ghcid tempDir = do
  dir <- liftIO Dir.getCurrentDirectory
  let dotGhci = tempDir FS.</> ".ghci"
  liftIO $ writeFile dotGhci $ unlines
    [ ":set prompt " <> prompt
    ]
  pb <- getPostBuild
  fsEvents <- watchDirectory (noDebounce FS.defaultConfig) (dir <$ pb)
  -- TODO Handle changes to "src" and ".cabal" differently. ":r" is only really appropriate
  -- when there are changes to ghcid.hs (this file) given the cabal repl command we're using.
  -- We could use ":show modules" to see which hs files are loaded and determine what to do based
  -- on that.
  let filteredFsEvents = flip ffilter fsEvents $ \e ->
        let subpath = fmap FS.splitPath $ List.stripPrefix dir $ FS.eventPath e
        in case subpath of
             Nothing -> False
             Just s -> not $ List.isPrefixOf ["/", ".git/"] s || List.isPrefixOf ["/", "dist/"] s
  batchedFsEvents <- batchOccurrences 0.1 filteredFsEvents
  let cabalRepl = (P.proc "cabal"
        [ "repl"
        , "ghcid"
        , "--ghc-options=-Wall"
        , "--ghc-options=-ignore-dot-ghci"
        , "--ghc-options=-ghci-script " <> dotGhci
        ]) { P.create_group = True }
  rec proc <- createProcess cabalRepl $ ProcessConfig
        { _processConfig_stdin = leftmost
            [ reload
            , fforMaybe (updated testMode) $ \case
                True -> Just "test"
                False -> Nothing
            ]
        , _processConfig_signal = never
        }
      let interruptible ls init = init && ls == LoadState_Loading
          requestInterrupt = gate (interruptible <$> current loadState <*> initialized) batchedFsEvents
      interrupt <- performEvent $ ffor requestInterrupt $
        const $ liftIO $ P.interruptProcessGroupOf $ _process_handle proc
      testMode <- holdDyn False $ leftmost
        [ False <$ requestInterrupt
        , False <$ reload
        , fforMaybe (updated loadState) $ \case
            LoadState_Succeeded -> Just True
            LoadState_Failed -> Just False
            _ -> Nothing
        ]
      let reload = ":r" <$ batchedFsEvents
      output <- foldDyn ($) "" $ leftmost
        [ flip mappend <$> _process_stdout proc
        , const "Reloading...\n" <$ batchedFsEvents
        ]
      let loadState = ffor output $ \o ->
            case reverse (C8.lines o) of
              (expectedPrompt:expectedMessage:_) ->
                if expectedPrompt == prompt
                  then if expectedMessage Regex.=~ ("Ok.*module.*loaded." :: BS.ByteString)
                    then LoadState_Succeeded
                    else LoadState_Failed
                  else LoadState_Loading
              _ -> LoadState_Loading
      initialized <- hold False $ fforMaybe (updated output) $ \o ->
        if o Regex.=~ ("GHCi, version.*: http://www.haskell.org/ghc/" :: BS.ByteString)
          then Just True
          else Nothing
  return (proc, batchedFsEvents, loadState, interrupt)
  where
    noDebounce :: FS.WatchConfig -> FS.WatchConfig
    noDebounce cfg = cfg { FS.confDebounce = FS.NoDebounce }

data Ghci t = Ghci
  { _ghci_moduleOut :: Event t BS.ByteString
  , _ghci_moduleErr :: Event t BS.ByteString
  , _ghci_execOut :: Event t BS.ByteString
  , _ghci_execErr :: Event t BS.ByteString
  , _ghci_filesystem :: Event t FS.Event
  , _ghci_loadState :: Dynamic t LoadState
  }

data LoadState
  = LoadState_Loading
  | LoadState_Failed
  | LoadState_Succeeded
  deriving (Show, Read, Eq, Ord)

main :: IO ()
main = withSystemTempDirectory "reflex-ghcid" $ \tempDir -> mainWidget $ do
  exit <- keyCombo (V.KChar 'c', [V.MCtrl])
  col $ do
    (p, fs, ready, interrupt) <- fixed 1 $ ghcid tempDir
    output <- foldDyn ($) "" $ leftmost
      [ flip mappend <$> _process_stdout p
      , flip mappend <$> _process_stderr p
      , const "" <$ fs
      ]
    fixed 3 $ boxStatic def $ text $ (<>) <$> "Status: " <*> ffor (current ready) (\case
      LoadState_Succeeded -> "Success!"
      LoadState_Failed -> "Failure!"
      LoadState_Loading -> "Loading...")
    stretch $ text $ T.decodeUtf8 <$> current output
    fixed 1 $ display <=< hold Nothing $ Just <$> _process_signal p
    fixed 1 $ display <=< hold Nothing $ leftmost [ Just <$> interrupt, Nothing <$ fs ]
  return $ () <$ exit

test :: IO ()
test = do
  let go :: Int -> IO ()
      go n = putStrLn ("Iteration No. " <> show n) >> threadDelay 1000000 >> go (n+1)
  go 1
  return ()

-- TODO
-- Wait until ghci loads before allowing any interrupts
