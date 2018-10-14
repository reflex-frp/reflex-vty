{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -threaded #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Time
import Graphics.Vty as V hiding (Event)
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty

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
  mainVtyWidget $ do
    inp <- input
    tellShutdown . fforMaybe inp $ \case
      V.EvKey V.KEsc _ -> Just ()
      _ -> Nothing

    size <- displaySize
    let region = fmap (\(w,h) -> Region 0 0 w h) size
        region1 = fmap (\(w,h) -> Region (w `div` 5) (h `div` 5) (w `div` 2) (h `div` 2)) size
        region2 = fmap (\(w,h) -> Region (w `div` 4) (h `div` 4) (2 * (w `div` 3)) (2*(h `div` 3))) size
        debugRegion = fmap (\(w, _) -> Region 0 0 w 3) size

    forM_ [region1, region2] $ \r -> pane r (constDyn False) $ testStringBox
    pane debugRegion (constDyn True) $ do
      lastEvent <- hold "" . fmap show =<< input
      tellImages . ffor (liftM2 (,) (current debugRegion) lastEvent) $ \(r, le) ->
        box r (\(w,_) -> wrapString w mempty le)


testStringBox :: (Reflex t, Monad m) => VtyWidget t m ()
testStringBox = do
  size <- displaySize
  tellImages . ffor (current size) $ \(w,h) ->
    box (Region 0 0 w h) (\_ -> wrapString w mempty . take 500 $ cycle ('\n' : ['a'..'z']))