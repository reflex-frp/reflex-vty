{-|
Module: Control.Monad.Writer.Adjustable
Description: An Adjustable instance for WriterT
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO find a better home for this
module Control.Monad.Writer.Adjustable where

import Control.Monad.Trans
import Control.Monad.Trans.Writer
import qualified Data.Dependent.Map as DMap
import Data.Foldable (fold)
import Data.Functor.Compose
import Reflex
import Reflex.Patch.DMapWithMove
import Reflex.Class.Switchable

instance (Adjustable t m, MonadHold t m, Switchable t w, Monoid w) => Adjustable t (WriterT w m) where
  runWithReplace a0 a' = do
    (w, r) <- lift $ runWithReplace (runWriterT a0) $ fmap (runWriterT) a'
    tell =<< switching (snd w) (snd <$> r)
    return $ (fst w, fst <$> r)
  traverseIntMapWithKeyWithAdjust f dm0 dm' = do
    (w, r) <- lift $ traverseIntMapWithKeyWithAdjust (\k v -> runWriterT (f k v)) dm0 dm'
    tell =<< switching (fold $ fmap snd w) (fmap (mconcat . patchIntMapNewElements . fmap snd) r)
    return (fmap fst w, fmap (fmap fst) r)
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    (w, r) <- lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> fmap (\(x, w') -> Compose (w', x)) $ runWriterT (f k v)) dm0 dm'
    let w' = runWriter $ DMap.traverseWithKey (\_ (Compose (w'', x)) -> tell w'' >> return x) w
        r' = fmap (runWriter . traversePatchDMapWithMove (\(Compose (w'', x)) -> tell w'' >> return x)) r
    tell =<< switching (snd w') (fmap snd r')
    return (fst w', fmap fst r')
