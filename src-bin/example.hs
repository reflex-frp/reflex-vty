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

{-
guest :: forall t m. VtyApp t m
guest e = do
  dr0@(w, h) <- displaySize
  dr <- holdDyn dr0 $ fforMaybe (e :: Event t VtyEvent) $ \case
    V.EvResize w h -> Just (w, h)
    _ -> Nothing
  let region = fmap (\(w,h) -> Region 0 0 w h) dr
      region1 = fmap (\(w,h) -> Region (w `div` 5) (h `div` 5) (w `div` 2) (h `div` 2)) dr
      region2 = fmap (\(w,h) -> Region (w `div` 4) (h `div` 4) (2 * (w `div` 3)) (2*(h `div` 3))) dr
      debugRegion = fmap (\(w, _) -> Region 0 0 w 3) dr
  events <- holdDyn "" $ show <$> e
  let debugOut = ffor (zipDyn events debugRegion) $ \(ev, r) ->
        box r $ \(w, _) -> wrapString w mempty ev

  let shutdown = fforMaybe e $ \case
        V.EvKey V.KEsc _ -> Just ()
        _ -> Nothing
      output = fmap V.picForLayers $
        current $ ffor ((,,) <$> region1 <*> region2 <*> debugOut) $ \(r1, r2, d) ->
          let b1 = box r1 (\(w, _) -> wrapString w mempty $ take 500 $ cycle ('\n' : ['a'..'z']))
              b2 = box r2 (\(w, _) -> wrapString w mempty $ take 500 $ cycle ('\n' : ['a'..'z']))
          in d ++ b2 ++ b1
  return $ VtyResult
    { _vtyResult_picture = output
    , _vtyResult_shutdown = shutdown
    }
-}

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
  pane region1 (constDyn False) . box $ debugInput
  pane region2 (constDyn True) . box $
    splitVDrag (box debugInput) (box dragTest)
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
testStringBox = box .
  string . pure . take 500 $ cycle ('\n' : ['a'..'z'])
