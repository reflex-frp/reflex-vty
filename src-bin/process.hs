{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex.Network
import Reflex.Process
import Reflex.Vty

import Control.Monad
import qualified Data.Text.Encoding as T
import qualified Graphics.Vty as V
import qualified System.Process as P

main :: IO ()
main = mainWidget $ do
  exit <- keyCombo (V.KChar 'c', [V.MCtrl])
  t <- tickLossyFromPostBuildTime 1
  col $ fixed 2 $ networkHold blank $ ffor t $ \t -> do
    (out, _) <- createProcess (P.proc "date" []) never
    col $ do
      fixed 1 $ text <=< hold "" $ T.decodeUtf8 <$> out
      fixed 1 $ display $ pure t
  return $ () <$ exit
