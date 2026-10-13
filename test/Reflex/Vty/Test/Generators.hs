{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description: Bounded QuickCheck generators and the fixed-seed property runner
--
-- Every property in the suite runs through 'runPlanProperty'. It runs
-- 300 cases with maximum size 50. It uses a fixed replay seed, so
-- failures are reproducible. Generated domains are bounded and valid.
-- Specs contain explicit boundary cases, not these generators.
module Reflex.Vty.Test.Generators
  ( runPlanProperty
  , planSeed
  , genDim
  , genPositiveDim
  , genAsciiChar
  , genAsciiText
  , genMixedWidthText
  , genLineText
  , genFiniteDouble
  , genEditScript
  , EditOp (..)
  ) where

import Control.Monad (forM)
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec (Expectation, expectationFailure)
import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)

-- | The fixed replay seed for the suite's properties.
planSeed :: Int
planSeed = 731947

-- | Run a property with the suite configuration: 300 cases, maximum
-- size 50, replay seed 'planSeed'. Failures report the seed and the
-- shrunk counterexample.
runPlanProperty :: Testable prop => prop -> Expectation
runPlanProperty prop = do
  r <-
    quickCheckWithResult
      stdArgs
        { maxSuccess = 300
        , maxSize = 50
        , replay = Just (mkQCGen planSeed, 0)
        , chatty = False
        }
      prop
  case r of
    Success {} -> pure ()
    _ -> expectationFailure ("[fixed replay seed " ++ show planSeed ++ "]\n" ++ output r)

-- | A nonnegative dimension up to 40.
genDim :: Gen Int
genDim = choose (0, 40)

-- | A positive dimension up to 40.
genPositiveDim :: Gen Int
genPositiveDim = choose (1, 40)

-- | A printable ASCII character (no control characters, no tabs).
genAsciiChar :: Gen Char
genAsciiChar = chr <$> choose (32, 126)

-- | ASCII text of bounded length (at most 200).
genAsciiText :: Gen Text
genAsciiText = T.pack <$> scale (min 200) (listOf genAsciiChar)

-- | One line of mixed-width content: ASCII, wide CJK, zero-width
-- combining marks, and spaces. There are no newlines. Line-oriented
-- APIs receive explicit newlines from 'genEditScript' inserts instead.
genLineText :: Gen Text
genLineText =
  fmap T.concat $
    scale (min 100) $
      listOf $
        frequency
          [ (6, T.singleton <$> genAsciiChar)
          , (2, pure "\30028")
          , (2, pure "\28450")
          , (1, pure "\769")
          , (1, pure " ")
          ]

-- | Multi-line mixed-width text (bounded).
genMixedWidthText :: Gen Text
genMixedWidthText = do
  ls <- scale (min 20) (listOf genLineText)
  pure (T.intercalate "\n" ls)

-- | A finite (non-NaN, non-infinite) double in [0, 2].
genFiniteDouble :: Gen Double
genFiniteDouble =
  suchThat
    (choose (0, 2))
    (\d -> not (isNaN d) && not (isInfinite d))

-- | Editing operations for zipper scripts.
data EditOp
  = OpLeft
  | OpRight
  | OpUp
  | OpDown
  | OpHome
  | OpEnd
  | OpPageUp !Int
  | OpPageDown !Int
  | OpInsertChar !Char
  | OpInsertNewline
  | OpInsertText !Text
  | OpDeleteLeft
  | OpDeleteRight
  | OpDeleteLeftWord
  | OpTab !Int
  | OpTop
  deriving (Eq, Show)

genEditOp :: Gen EditOp
genEditOp =
  frequency
    [ (3, pure OpLeft)
    , (3, pure OpRight)
    , (2, pure OpUp)
    , (2, pure OpDown)
    , (2, pure OpHome)
    , (2, pure OpEnd)
    , (1, OpPageUp <$> choose (-1, 3))
    , (1, OpPageDown <$> choose (-1, 3))
    , (6, OpInsertChar <$> genAsciiChar)
    , (2, pure OpInsertNewline)
    , (2, OpInsertText <$> scale (min 20) genLineText)
    , (3, pure OpDeleteLeft)
    , (3, pure OpDeleteRight)
    , (2, pure OpDeleteLeftWord)
    , (1, OpTab <$> choose (0, 8))
    , (1, pure OpTop)
    ]

-- | An edit script of at most 100 operations.
genEditScript :: Gen [EditOp]
genEditScript = scale (min 100) (listOf genEditOp)
