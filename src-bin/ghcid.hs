{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex.Process
import Reflex.Vty

import Control.Concurrent
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as List
import Data.Sequence (Seq)
import Data.String (IsString)
import qualified Data.Text.Encoding as T
import qualified Graphics.Vty as V
import Text.Regex.TDFA as Regex
import Text.Regex.TDFA.ByteString ()
import qualified System.Directory as Dir
import qualified System.FilePath.Posix as FS
import qualified System.FSNotify as FS
import System.IO.Temp (withSystemTempDirectory)
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
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , PostBuild t m
     , MonadIO m
     , MonadFix m
     , MonadHold t m
     )
  => FilePath
  -> m (Ghci t)
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
            , fmap (const "test") $ ffilter id $ updated testMode
            ]
        , _processConfig_signal = never
        }
      let interruptible ls ready = ready && ls == LoadState_Loading
          requestInterrupt = gate (interruptible <$> current loadState <*> initialized) batchedFsEvents
      performEvent_ $ ffor requestInterrupt $
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
  return $ Ghci
    { _ghci_moduleOut = gate (not <$> current testMode) $ _process_stdout proc
    , _ghci_moduleErr = gate (not <$> current testMode) $ _process_stderr proc
    , _ghci_execOut = gate (current testMode) $ _process_stdout proc
    , _ghci_execErr = gate (current testMode) $ _process_stderr proc
    , _ghci_filesystem = batchedFsEvents
    , _ghci_status = (\ls t -> if t then Status_Exec else Status_Load ls) <$> loadState <*> testMode
    }
  where
    noDebounce :: FS.WatchConfig -> FS.WatchConfig
    noDebounce cfg = cfg { FS.confDebounce = FS.NoDebounce }

data Ghci t = Ghci
  { _ghci_moduleOut :: Event t BS.ByteString
  , _ghci_moduleErr :: Event t BS.ByteString
  , _ghci_execOut :: Event t BS.ByteString
  , _ghci_execErr :: Event t BS.ByteString
  , _ghci_filesystem :: Event t (Seq FS.Event)
  , _ghci_status :: Dynamic t Status
  }

data Status = Status_Load LoadState
            | Status_Exec
  deriving (Show, Read, Eq, Ord)

data LoadState
  = LoadState_Loading
  | LoadState_Failed
  | LoadState_Succeeded
  deriving (Show, Read, Eq, Ord)

main :: IO ()
main = withSystemTempDirectory "reflex-ghcid" $ \tempDir -> mainWidget $ do
  exit <- keyCombo (V.KChar 'c', [V.MCtrl])
  ghci <- ghcid tempDir
  let ghciLoadStatus = col $ do
        fixed 3 $ boxStatic def $ text $ (<>) <$> "Status: " <*> ffor (current $ _ghci_status ghci) (\case
          Status_Exec -> "Running..."
          Status_Load LoadState_Succeeded -> "Success!"
          Status_Load LoadState_Failed -> "Failure!"
          Status_Load LoadState_Loading -> "Loading...")
        out <- foldDyn ($) "" $ leftmost
          [ flip mappend <$> _ghci_moduleOut ghci
          , flip mappend <$> _ghci_moduleErr ghci
          , const "" <$ _ghci_filesystem ghci
          ]
        stretch $ text $ T.decodeUtf8 <$> current out
      ghciExecOutput = do
        out <- foldDyn ($) "" $ leftmost
          [ flip mappend <$> _ghci_execOut ghci
          , flip mappend <$> _ghci_execErr ghci
          , const "" <$ _ghci_filesystem ghci
          ]
        text $ T.decodeUtf8 <$> current out
  _ <- splitVDrag (hRule doubleBoxStyle) ghciLoadStatus ghciExecOutput
  return $ () <$ exit

test :: IO ()
test = do
  let go :: Int -> IO ()
      go n = putStrLn ("Iteration No. " <> show n) >> threadDelay 1000000 >> go (n+1)
  go 1
