-- | A gallery of small, standalone reflex-vty demos — ports of the Bubble Tea
-- examples (https://github.com/charmbracelet/bubbletea/tree/main/examples).
--
-- Run one with: @cabal run examples -- <name>@ (defaults to @spinner@).
module Main where

import qualified Data.Text as T
import Reflex
import System.Environment (getArgs)

import Example.CellBuffer
import Example.Common
import Example.DoomFire
import Example.Fullscreen
import Example.Pager
import Example.PreventQuit
import Example.PrintKey
import Example.Progress
import Example.Realtime
import Example.Simple
import Example.Space
import Example.Spinner
import Example.Splash
import Example.Stopwatch
import Example.TextInput
import Example.Timer
import Example.Views
import Reflex.Vty

main :: IO ()
main = do
  args <- getArgs
  let name = case args of
        (a : _) -> a
        _ -> "spinner"
  mainWidget def $ runExample name

-- | Dispatch to a demo by name.
runExample :: (Demo t m, HasScreenMode t m) => String -> m (Event t ())
runExample = \case
  "spinner" -> spinner
  "prevent-quit" -> preventQuit
  "print-key" -> printKey
  "realtime" -> realtime
  "simple" -> simple
  "fullscreen" -> fullscreen
  "progress" -> progressBar
  "progress-bar" -> progressBar
  "stopwatch" -> stopwatch
  "timer" -> timer
  "textinput" -> textInputExample
  "pager" -> pager
  "views" -> views
  "cellbuffer" -> cellBuffer
  "doom-fire" -> doomFire
  "space" -> space
  "splash" -> splash
  other -> do
    text $ pure $ "Unknown example: " <> T.pack other <> "  (q to quit)"
    quit
