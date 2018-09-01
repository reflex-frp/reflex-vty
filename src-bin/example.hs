{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Time
import Graphics.Vty as V
import Reflex
import Reflex.Vty

data Region = Region
  { _region_left :: Int
  , _region_top :: Int
  , _region_width :: Int
  , _region_height :: Int
  }
  deriving (Show, Read, Eq, Ord)

regionToDisplayRegion :: Region -> DisplayRegion
regionToDisplayRegion (Region _ _ w h) = (w, h)

region :: Region -> (DisplayRegion -> Image) -> [Image]
region r@(Region left top width height) i =
  [ within r $ resize width height $ i (width, height)
  , within r $ wrapString width defAttr $ replicate (width * height) ' '
  ]

within :: Region -> Image -> Image
within (Region left top width height) =
  translate left top . V.crop width height

wrapString :: Int -> Attr -> String -> Image
wrapString maxWidth attrs = vertCat . concatMap (fmap (string attrs) . fmap (take maxWidth) . takeWhile (not . null) . iterate (drop maxWidth)) . lines

box :: Region -> (DisplayRegion -> Image) -> [Image]
box r@(Region left top width height) i =
  let hBorder = string mempty $ replicate width '-'
      vBorder = wrapString 1 mempty $ replicate (height - 2) '|'
      internal = Region
        { _region_left = left + 1
        , _region_top = top + 1
        , _region_width = width - 2
        , _region_height = height - 2
        }
  in  [ within (r { _region_height = 1 }) hBorder
      , within (Region left (top + 1) 1 (height - 2)) vBorder
      , within (Region (left + width - 1) (top + 1) 1 (height - 2)) vBorder
      , within (r { _region_top = top + height - 1 }) hBorder
      ] ++ region internal i

guest :: forall t m. VtyApp t m
guest e = do
  dr0@(w, h) <- displayRegion
  dr <- holdDyn dr0 $ fforMaybe e $ \case
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

main :: IO ()
main = runVtyApp guest
