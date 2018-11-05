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
    (result0, result) <- lift $ runWithReplace (runWriterT a0) $ fmap (runWriterT) a'
    tell =<< switching (snd result0) (snd <$> result)
    return $ (fst result0, fst <$> result)
  traverseIntMapWithKeyWithAdjust f dm0 dm' = do
    (result0, result) <- lift $ traverseIntMapWithKeyWithAdjust (\k v -> runWriterT (f k v)) dm0 dm'
    tell =<< switching (fold $ fmap snd result0) (fmap (mconcat . patchIntMapNewElements . fmap snd) result)
    return (fmap fst result0, fmap (fmap fst) result)
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    (result0, result) <- lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> fmap (\(r, r0) -> Compose (r0, r)) $ runWriterT (f k v)) dm0 dm'
    let result0' = runWriter $ DMap.traverseWithKey (\_ (Compose (w'', x)) -> tell w'' >> return x) result0
        result' = fmap (runWriter . traversePatchDMapWithMove (\(Compose (w'', x)) -> tell w'' >> return x)) result
    tell =<< switching (snd result0') (fmap snd result')
    return (fst result0', fmap fst result')
