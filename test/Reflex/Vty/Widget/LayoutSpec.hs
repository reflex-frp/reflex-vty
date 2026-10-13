{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description: Pure layout-solver allocation semantics.
--
-- Expectations are independent literals and invariant properties; the
-- production allocation algorithm is never re-implemented here.
module Reflex.Vty.Widget.LayoutSpec (spec) where

import Control.Monad (forM_, replicateM)
import qualified Data.Map.Ordered as OM
import Test.Hspec
import Test.QuickCheck (Gen, choose, conjoin, counterexample, forAll, ioProperty, listOf, oneof, scale)

import Control.Monad.NodeId (NodeId, getNextNodeId, runNodeIdT)
import Reflex.Vty.Test.Generators (runPlanProperty)
import Reflex.Vty.Widget (Region (..))
import Reflex.Vty.Widget.Layout

-- | A row/column forest from main-axis constraints, using fresh node
-- identifiers from a real allocator. Returns the identifiers in
-- insertion order so tests can extract chunks by key (the underlying
-- map's traversal order is construction-dependent and must not be
-- relied on).
mkForest :: [Constraint] -> IO ([NodeId], LayoutForest (Constraint, Orientation))
mkForest cs = do
  ids <- runNodeIdT (replicateM (length cs) getNextNodeId)
  pure
    ( ids
    , fromListLF
        [ (nid, LayoutTree (c, Orientation_Row) mempty)
        | (nid, c) <- zip ids cs
        ]
    )

-- | The solved (offset, size) pairs of the direct children, in the
-- given key order.
childChunksIn
  :: Orientation
  -> Region
  -> [NodeId]
  -> LayoutTree (Region, Orientation)
  -> [(Int, Int)]
childChunksIn o parent ids = chunksInForest o parent ids . childrenLT

chunksInForest
  :: Orientation
  -> Region
  -> [NodeId]
  -> LayoutForest (Region, Orientation)
  -> [(Int, Int)]
chunksInForest o parent ids f =
  [ mainAxis o (fst (rootLT child))
  | i <- ids
  , Just child <- [lookupLF i f]
  ]
  where
    mainAxis orient r = case orient of
      Orientation_Row -> (_region_left r - _region_left parent, _region_width r)
      Orientation_Column -> (_region_top r - _region_top parent, _region_height r)

sizesOf :: [(Int, Int)] -> [Int]
sizesOf = map snd

spec :: Spec
spec = describe "Reflex.Vty.Widget.Layout (pure solver)" $ do
  describe "solve" $ do
    it "row width 12 with fixed 5, min 3, min 3 allocates 5/4/3" $ do
      (ids, forest) <- mkForest [Constraint_Fixed 5, Constraint_Min 3, Constraint_Min 3]
      let tree = solve Orientation_Row (Region 0 0 12 1) forest
      childChunksIn Orientation_Row (Region 0 0 12 1) ids tree `shouldBe` [(0, 5), (5, 4), (9, 3)]

    it "fixed-only children keep their sizes and ignore leftover space" $ do
      (ids, forest) <- mkForest [Constraint_Fixed 2, Constraint_Fixed 7]
      let tree = solve Orientation_Row (Region 0 0 20 1) forest
      childChunksIn Orientation_Row (Region 0 0 20 1) ids tree `shouldBe` [(0, 2), (2, 7)]

    it "an empty forest yields the root region with no children" $ do
      (_, forest) <- mkForest []
      let r = Region 3 4 10 10
          tree = solve Orientation_Column r forest
      rootLT tree `shouldBe` (r, Orientation_Column)
      length (OM.assocs (unLayoutForest (childrenLT tree))) `shouldBe` 0

    it "zero extent overflows deliberately with minimum sizes intact" $ do
      (ids, forest) <- mkForest [Constraint_Fixed 2, Constraint_Min 1]
      let tree = solve Orientation_Row (Region 0 0 0 1) forest
      sizesOf (childChunksIn Orientation_Row (Region 0 0 0 1) ids tree) `shouldBe` [2, 1]

    it "distributes remainders to the earliest stretchable children" $ do
      (ids, forest) <- mkForest [Constraint_Min 0, Constraint_Min 0, Constraint_Min 0]
      let tree = solve Orientation_Row (Region 0 0 10 1) forest
      childChunksIn Orientation_Row (Region 0 0 10 1) ids tree `shouldBe` [(0, 4), (4, 3), (7, 3)]

    it "nests orientations: a row child's column chunks its width" $ do
      (innerIds, innerForest) <- mkForest [Constraint_Min 1, Constraint_Min 1]
      [innerKey, lastKey] <- runNodeIdT (replicateM 2 getNextNodeId)
      let inner = LayoutTree (Constraint_Fixed 6, Orientation_Column) innerForest
          forest = fromListLF [(innerKey, inner), (lastKey, LayoutTree (Constraint_Min 0, Orientation_Row) mempty)]
          tree = solve Orientation_Row (Region 0 0 10 5) forest
          [rowChild0, rowChild1] = OM.assocs (unLayoutForest (childrenLT tree))
      -- The fixed 6-wide first child gets the column layout inside it.
      case lookupLF innerKey (childrenLT tree) of
        Just (LayoutTree (r0, _) innerSolved) -> do
          _region_left r0 `shouldBe` 0
          _region_width r0 `shouldBe` 6
          chunksInForest Orientation_Column r0 innerIds innerSolved
            `shouldBe` [(0, 3), (3, 2)]
        _ -> expectationFailure "expected the fixed column child"
      case lookupLF lastKey (childrenLT tree) of
        Just (LayoutTree (r1, _) _) -> do
          _region_left r1 `shouldBe` 6
          _region_width r1 `shouldBe` 4
        _ -> expectationFailure "expected the stretchable row child"

    it "preserves solver invariants on random constraint forests" $
      runPlanProperty $
        conjoin $
          map
            ( \o ->
                forAll ((,) <$> choose (0, 40) <*> scale (min 6) constraintList) $ \(extent, cs) -> ioProperty $ do
                  (ids, forest) <- mkForest cs
                  let region = case o of
                        Orientation_Row -> Region 0 0 extent 1
                        Orientation_Column -> Region 0 0 1 extent
                      tree = solve o region forest
                      chunks = childChunksIn o region ids tree
                      sizes = sizesOf chunks
                      offsets = map fst chunks
                      minTotal = sum [n | c <- cs, n <- [constraintMin c]]
                      numStretch = length [() | Constraint_Min _ <- cs]
                      fits = minTotal <= extent
                      expectedTotal
                        | not fits = minTotal -- deliberate overflow
                        | numStretch > 0 = extent -- stretch absorbs everything
                        | otherwise = minTotal -- fixed-only leaves leftover
                  pure $
                    counterexample
                      ( show o
                          ++ " extent "
                          ++ show extent
                          ++ " constraints "
                          ++ show cs
                          ++ " -> chunks "
                          ++ show chunks
                      )
                      ( length sizes == length cs
                          && and (zipWith (\c s -> s >= constraintMin c) cs sizes)
                          && and (zipWith (<=) offsets (drop 1 offsets))
                          && and (zipWith (\(_, s) next -> next - s >= 0) chunks (drop 1 offsets))
                          && sum sizes == expectedTotal
                      )
            )
            orientations

    it "column chunks advance top, row chunks advance left" $ do
      (ids, forest) <- mkForest [Constraint_Fixed 4, Constraint_Fixed 5]
      let parent = Region 2 3 10 9
          colTree = solve Orientation_Column parent forest
      childChunksIn Orientation_Column parent ids colTree `shouldBe` [(0, 4), (4, 5)]
      case map (\node -> lookupLF node (childrenLT colTree)) (take 1 ids) of
        [Just (LayoutTree (r0, _) _)] -> (_region_left r0, _region_top r0, _region_height r0) `shouldBe` (2, 3, 4)
        _ -> expectationFailure "expected the first column child"
  where
    orientations = [Orientation_Row, Orientation_Column]
    offsetPlus s next = next - s >= 0
    constraintMin c = case c of
      Constraint_Fixed n -> n
      Constraint_Min n -> n

-- | Bounded valid constraint lists (up to 6 children).
constraintList :: Gen [Constraint]
constraintList =
  scale (min 6) $
    listOf $
      oneof
        [ Constraint_Fixed <$> choose (0, 12)
        , Constraint_Min <$> choose (0, 12)
        ]
