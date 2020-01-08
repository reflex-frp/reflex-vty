{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex.Process
import Reflex.Vty

import Control.Concurrent
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.List as List
import Data.Sequence (Seq)
import qualified Data.Text.Encoding as T
import qualified Graphics.Vty as V
import qualified System.Directory as Dir
import qualified System.FilePath.Posix as FS
import qualified System.FSNotify as FS
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

ghcid
  :: (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m), PostBuild t m, MonadIO m, MonadFix m, MonadHold t m)
  => m (Process t, Event t (Seq FS.Event))
ghcid = do
  dir <- liftIO Dir.getCurrentDirectory
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
  proc <- createProcess (P.proc "cabal" ["repl", "ghcid", "--ghc-options=-Wall"]) $ ffor batchedFsEvents $ \_ -> ":r"
  return (proc, batchedFsEvents)
  where
    noDebounce :: FS.WatchConfig -> FS.WatchConfig
    noDebounce cfg = cfg { FS.confDebounce = FS.NoDebounce }

main :: IO ()
main = mainWidget $ do
  exit <- keyCombo (V.KChar 'c', [V.MCtrl])
  (p, fs) <- ghcid
  col $ do
    output <- foldDyn ($) "" $ leftmost
      [ flip mappend <$> _process_stdout p 
      , flip mappend <$> _process_stderr p
      , const "" <$ fs
      ]
    stretch $ text $ T.decodeUtf8 <$> current output
  return $ () <$ exit
