{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- Space-leak repro for profiling. A streaming producer fires into the FRP
-- network as fast as possible via 'performEventAsync'; the only buffer in
-- play is the reflex host's event channel. The program self-terminates after
-- a short delay via 'exitSuccess' so the RTS flushes profiling output.
--
-- Uses @forever@ (not @forM_ [1..n]@) so the producer itself doesn't
-- materialize a list -- keeping the profile focused on the host's event
-- channel rather than the producer's enumeration.
--
-- Run under the threaded RTS with >=2 capabilities, e.g.:
--   leaktest +RTS -N2 -hy -p -M4000m -RTS
module Main (main) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import qualified Graphics.Vty as V
import Reflex
import System.Exit (exitSuccess)

import Data.Text.Zipper (TextAlignment (..))
import Reflex.Vty

main :: IO ()
main = do
  void $ Async.async (threadDelay 2000000 >> exitSuccess)
  mainWidget def $ initManager_ $ do
    setupE <- getPostBuild
    inp <- input
    sE <-
      performEventAsync $
        ffor setupE $
          \() fire -> liftIO . void . Async.async $ forever (fire (0 :: Int))
    sB <- current <$> holdDyn 0 sE
    col $
      grout (fixed 17) $
        boxTitle (constant TextAlignment_Left) (constant doubleBoxStyle) "" $
          grout (fixed 1) $
            text (pack . show <$> sB)
    pure $ fforMaybe inp $ \case
      V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
      _ -> Nothing
