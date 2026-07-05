module Main (main) where

import App (greeting)
import Reflex.Vty

main :: IO ()
main = mainWidget def $ do
  text (pure greeting)
  ctrlc
