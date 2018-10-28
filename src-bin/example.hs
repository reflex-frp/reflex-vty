{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -threaded #-}

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Time
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty
import Reflex.Vty.Widget

main :: IO ()
main =
  mainWidget $ do
    inp <- input
    tellShutdown . fforMaybe inp $ \case
      V.EvKey V.KEsc _ -> Just ()
      _ -> Nothing

    debugInput
    testBoxes
    return ()

testBoxes :: (Reflex t, MonadHold t m, MonadFix m) => VtyWidget t m ()
testBoxes = do 
  size <- displaySize
  let region1 = fmap (\(w,h) -> Region (w `div` 6) (h `div` 6) (w `div` 2) (h `div` 2)) size
      region2 = fmap (\(w,h) -> Region (w `div` 4) (h `div` 4) (2 * (w `div` 3)) (2*(h `div` 3))) size
  pane region1 (constDyn False) . box singleBoxStyle $ debugInput
  pane region2 (constDyn True) . box singleBoxStyle $
    splitVDrag (hRule doubleBoxStyle) (box roundedBoxStyle debugInput) (box roundedBoxStyle dragTest)
  return ()

debugFocus :: (Reflex t, Monad m) => VtyWidget t m ()
debugFocus = do
  f <- focus
  string $ show <$> current f

debugInput :: (Reflex t, MonadHold t m) => VtyWidget t m ()
debugInput = do
  lastEvent <- hold "No event yet" . fmap show =<< input
  string lastEvent

dragTest :: (Reflex t, MonadHold t m, MonadFix m) => VtyWidget t m ()
dragTest = do
  lastEvent <- hold "No event yet" . fmap show =<< drag V.BLeft
  string lastEvent

testStringBox :: (Reflex t, Monad m) => VtyWidget t m ()
testStringBox = box singleBoxStyle .
  string . pure . take 500 $ cycle ('\n' : ['a'..'z'])
