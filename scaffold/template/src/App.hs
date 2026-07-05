{-# LANGUAGE OverloadedStrings #-}
module App (greeting) where

import Data.Text (Text)

greeting :: Text
greeting = "Hello from @PACKAGE_NAME@! Press Ctrl+C to quit."
