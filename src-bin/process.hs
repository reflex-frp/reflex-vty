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
  col $ do
    out <- fmap (switch . current) $ fixed 2 $ networkHold (return never) $ ffor t $ \t -> do
      (out, _) <- createProcess (P.proc "date" []) never
      row $ do
        fixed 10 $ text $ pure "Tick:"
        stretch $ display $ pure t
      return out
    fixed 1 $ do
      row $ do
        fixed 10 $ text "stdout:"
        stretch $ text <=< hold "" $ T.decodeUtf8 <$> out
  return $ () <$ exit
