{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad.IO.Class
import Data.Time
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty

guest :: forall t m. VtyApp t m
guest e = do
  now <- liftIO getCurrentTime
  ticks <- fmap show <$> tickLossy 1 now
  let shutdown = fforMaybe e $ \case
        V.EvKey V.KEsc _ -> Just ()
        _ -> Nothing
  picture <- hold (V.picForImage $ V.string mempty "Initial") $ V.picForImage . V.string mempty <$>
    leftmost [show <$> e, show <$> ticks]
  return $ VtyResult
    { _vtyResult_picture = picture
    , _vtyResult_refresh = never
    , _vtyResult_shutdown = shutdown
    }

main :: IO ()
main = host guest
