-- |
-- Description: Uniqueness of node identifiers within one allocation scope.
--
-- Identifiers are unique only within a single 'runNodeIdT' scope.
-- This includes allocations during tree replacement; the widget-level
-- specs exercise those through the production 'Adjustable' instances.
-- The printed representation, global uniqueness, and the first
-- identifier value are unspecified.
module Control.Monad.NodeIdSpec (spec) where

import Control.Monad (replicateM)
import Data.List (nub)
import Test.Hspec
import Test.QuickCheck (choose, counterexample, forAll, ioProperty)

import Control.Monad.NodeId
import Reflex.Vty.Test.Generators (runPlanProperty)

spec :: Spec
spec = describe "Control.Monad.NodeId" $ do
  it "allocates unique identifiers within one runNodeIdT" $
    runPlanProperty $
      forAll (choose (1, 300)) $ \n ->
        ioProperty $ do
          ids <- runNodeIdT (replicateM n getNextNodeId)
          pure $
            counterexample
              ("expected " ++ show n ++ " distinct ids, got " ++ show (length (nub ids)))
              (length (nub ids) == n)
