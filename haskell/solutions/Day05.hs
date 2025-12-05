{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day05 where

import AOC.Solution

import ParsingPrelude
import Util
import Data.Interval ((<=..<=), Extended (Finite), width)
import qualified Data.IntervalSet as IntervalSet
import Data.IntervalSet (IntervalSet)

solution :: Solution (IntervalSet Int, [Int]) Int Int
solution = Solution
  { decodeInput = (,) <$> (fmap toIvalSet $ rangeP `sepEndBy` eol) <* eol <*> decimal `sepBy` space1
  , solveA = defSolver
    { solve = Just . countFresh
    }
  , solveB = defSolver
    { solve = Just . ivalSetSize . fst

    }
  , tests =
    [ unlines
      [ "3-5"
      , "10-14"
      , "16-20"
      , "12-18"
      , ""
      , "1"
      , "5"
      , "8"
      , "11"
      , "17"
      , "32"
      ] :=> [(PartA, "3"), (PartB, "14")]

    ]
  }

rangeP :: Parser (Int, Int)
rangeP = (,) <$> decimal <* "-" <*> decimal

countFresh :: (IntervalSet Int, [Int]) -> Int
countFresh (freshSet, ids) = countHits (`IntervalSet.member` freshSet) ids

toIvalSet :: Ord a => [(a, a)] -> IntervalSet a
toIvalSet xs = IntervalSet.fromList [Finite l <=..<= Finite u | (l, u) <- xs]

ivalSetSize :: IntervalSet Int -> Int
ivalSetSize ivs = sum [width i + 1 | i <- IntervalSet.toList ivs]