{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import Reflex.Network (networkHold)
import Text.Regex.TDFA as Regex
import Text.Regex.TDFA.ByteString ()
import qualified System.Directory as Dir
import qualified System.FilePath.Posix as FS
import qualified System.FSNotify as FS
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Process as P

-- TODO: Move this to a common module
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
  -- ^ Temporary directory where we can put a ".ghci" file
  -> m (Ghci t)
ghcid tempDir = do
  dir <- liftIO Dir.getCurrentDirectory
  -- Create a .ghci file with our custom settings
  -- TODO: Should the .ghci file be customizable by users of this function?
  let dotGhci = tempDir FS.</> ".ghci"
  liftIO $ writeFile dotGhci $ unlines
    [ ":set prompt " <> prompt
    ]
  pb <- getPostBuild

  -- Watch the project directory for changes
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
  -- Events are batched because otherwise we'd get several updates corresponding to one
  -- user-level change. For example, saving a file in vim results in an event claiming
  -- the file was removed followed almost immediately by an event adding the file
  batchedFsEvents <- batchOccurrences 0.1 filteredFsEvents

  -- Construct the cabal repl command line
  -- TODO: This should be configurable
  let cabalRepl = (P.proc "cabal"
        [ "repl"
        , "ghcid"
        , "--ghc-options=-Wall"
        , "--ghc-options=-ignore-dot-ghci"
        , "--ghc-options=-ghci-script " <> dotGhci
        ]) { P.create_group = True } -- It's important that we're in our own process group
                                     -- so that we can interrupt without killing the parent
                                     -- (vty) process

  -- Request a reload every time the files we're watching change
  let reload = ":r" <$ batchedFsEvents

  -- Run the process and feed it some input:
  rec proc <- createProcess cabalRepl $ ProcessConfig
        { _processConfig_stdin = leftmost
            [ reload
            -- Execute some expression if ghci is ready to receive it
            -- TODO: This should be configurable
            , fmap (const "test") $ ffilter id $ updated testMode
            ]
        , _processConfig_signal = never
        }

      -- Capture and accumulate stdout between reloads. We'll inspect this value
      -- to determine ghci's state
      output <- foldDyn ($) "" $ leftmost
        [ flip mappend <$> _process_stdout proc
        , const "Reloading...\n" <$ batchedFsEvents
        ]

      -- We need to know when ghci is initialized enough that it won't die when
      -- it receives a sigint. We wait to see the version line in the output as
      -- a proxy for ghci's readiness to be interrupted
      initialized <- hold False $ fforMaybe (updated output) $ \o ->
        if o Regex.=~ ("GHCi, version.*: http://www.haskell.org/ghc/" :: BS.ByteString)
          then Just True
          else Nothing

     -- Only interrupt when there's a file change and we're ready and not in an idle state
      let interruptible ls ready = ready && ls == LoadState_Loading
          requestInterrupt = gate (interruptible <$> current loadState <*> initialized) batchedFsEvents
      performEvent_ $ ffor requestInterrupt $
        const $ liftIO $ P.interruptProcessGroupOf $ _process_handle proc

      -- Keep track of whether we're in a position from which an expression can be evaluated
      -- We enter this mode upon successfully loading the requested modules.
      -- TODO: What if the expression fails?
      testMode <- holdDyn False $ leftmost
        [ False <$ requestInterrupt
        , False <$ reload
        , fforMaybe (updated loadState) $ \case
            LoadState_Succeeded -> Just True
            LoadState_Failed -> Just False
            _ -> Nothing
        ]

      -- Inspect the output and determine what state ghci is in
      let loadState = ffor output $ \o ->
            case reverse (C8.lines o) of
              (expectedPrompt:expectedMessage:_) ->
                if expectedPrompt == prompt
                  then if expectedMessage Regex.=~ ("Ok.*module.*loaded." :: BS.ByteString)
                    then LoadState_Succeeded
                    else LoadState_Failed
                  else LoadState_Loading
              _ -> LoadState_Loading

  -- Below, we split up the output of the ghci process into things that ghci itself
  -- produces (e.g., errors, warnings, loading messages) and the output of the expression
  -- it is evaluating
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
        (dh, scroll) <- stretch $ do
          dh <- displayHeight
          scroll <- scrollableText never $ T.decodeUtf8 <$> current out
          return (dh, scroll)
        fixed 1 $ text $ (\h (ix, n) -> if n - ix + 1 > h then "↓ More ↓" else "") <$> current dh <*> scroll
      ghciExecOutput = do
        out <- fmap (fmap T.decodeUtf8) $ foldDyn ($) "" $ leftmost
          [ flip mappend <$> _ghci_execOut ghci
          , flip mappend <$> _ghci_execErr ghci
          , const "" <$ _ghci_filesystem ghci
          ]
        let scrollingOutput = do
              dh <- displayHeight
              rec scroll <- scrollableText (tagMaybe (scrollBy <$> current dh <*> scroll) $ updated out) $ current out
                  let scrollBy h (ix, n) =
                        if | ix == 0 && n <= h -> Nothing -- Scrolled to the top and we don't have to scroll down
                           | n > h && n - ix - h == 0 -> Just 1
                           | otherwise -> Nothing
              return ()
        -- Rebuild the entire output widget so that we don't have to worry about resetting scroll state
        _ <- networkHold scrollingOutput $ ffor (_ghci_filesystem ghci) $ \_ -> scrollingOutput
        return ()
  _ <- splitVDrag (hRule doubleBoxStyle) ghciLoadStatus ghciExecOutput
  return $ () <$ exit

test :: IO ()
test = do
  let go :: Int -> IO ()
      go n = putStrLn ("Iteration No. " <> show n) >> threadDelay 1000000 >> go (n+1)
  go 1
