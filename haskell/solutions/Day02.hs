{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day02 where

import AOC.Solution

import ParsingPrelude
import Math.NumberTheory.Logarithms
import Data.Set (Set)
import qualified Data.Set as Set

solution :: Solution [(Integer, Integer)] [Integer] (Set Integer)
solution = Solution
  { decodeInput = ((,) <$> decimal <* "-" <*> decimal) `sepBy` ","
  , solveA = Solver
    { solve = Just . concatMap simplyInvalidIdsInRange
    , display = show . sum
    }
  , solveB = Solver
    { solve = Just . Set.unions . fmap allInvalidIdsInRange
    , display = show . sum
    }
  , tests =
    [ "11-22" :=> [(PartA, "33"), (PartB, "33")]
    , "95-115" :=> [(PartA, "99"), (PartB, show (99+111))]
    , "998-1012" :=> [(PartA, "1010"), (PartB, show (999+1010))]
    , "1188511880-1188511890" :=> [(PartA, "1188511885"), (PartB, "1188511885")]
    , "222220-222224" :=> [(PartA, "222222"), (PartB, "222222")]
    , "1698522-1698528" :=> [(PartA, "0"), (PartB, "0")]
    , "446443-446449" :=> [(PartA, "446446"), (PartB, "446446")]
    , "38593856-38593862" :=> [(PartA, "38593859"), (PartB, "38593859")]
    , "565653-565659,824824821-824824827,2121212118-2121212124"
      :=> [(PartA, "0")]
    , "565653-565659" :=> [(PartB, "565656")]
    , "824824821-824824827" :=> [(PartB, "824824824")]
    , "2121212118-2121212124" :=> [(PartB, "2121212121")]
    , "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
      :=> [(PartA, "1227775554"), (PartB, "4174379265")]
    ]
  }

simplyInvalidIdsInRange :: (Integer, Integer) -> [Integer]
simplyInvalidIdsInRange (lo, hi) = let
  subranges = splitSubranges lo hi
  in concatMap simplyinvalidIdsInSubrange subranges

-- split the range (x, y) into subranges of constant digit count
splitSubranges :: Integer -> Integer -> [(Int, Integer, Integer)]
splitSubranges lo hi
  | lo > hi = []
  | otherwise = (digCount, lo, mid) : splitSubranges (mid+1) hi
  where
    digCount = integerLog10 lo + 1
    mid = min hi (10^digCount - 1)

-- only 2x repetitions (part A)
simplyinvalidIdsInSubrange :: (Int, Integer, Integer) -> [Integer]
simplyinvalidIdsInSubrange (digits, lo, hi)
  | odd digits = []
  | otherwise
  = let
    halfwidth = digits `div` 2
    prefix = lo `div` 10^halfwidth
  in takeWhile (<= hi) $ dropWhile (< lo) [p * 10^halfwidth + p | p <- [prefix ..]]

allInvalidIdsInRange :: (Integer, Integer) -> Set Integer
allInvalidIdsInRange (lo, hi) = let
  subranges = splitSubranges lo hi
  in Set.unions $ allInvalidIdsInSubrange <$> subranges

allInvalidIdsInSubrange :: (Int, Integer, Integer) -> Set Integer
allInvalidIdsInSubrange (digits, lo, hi) = Set.unions [invalidByRepetitions r digits lo hi | r <- [2 .. digits]]

invalidByRepetitions :: Int -> Int -> Integer -> Integer -> Set Integer
invalidByRepetitions reps digits lo hi
  | digits `mod` reps /= 0 = Set.empty
  | otherwise = let
      unitSize = digits `div` reps
      prefix = lo `div` 10 ^ (digits - unitSize)
      mkRepeat x = x * sum [10^e | e <- [0, unitSize .. digits-unitSize] ]
    in Set.fromAscList $ takeWhile (<= hi) $ dropWhile (< lo) [mkRepeat p | p <- [prefix ..]]