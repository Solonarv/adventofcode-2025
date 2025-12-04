{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day03 where

import AOC.Solution

import ParsingPrelude
import Util
import Data.Vector (Vector)
import qualified Data.Vector as Vector

solution :: Solution [Vector Int] Int Int
solution = Solution
  { decodeInput = (Vector.fromList <$> some (singleDigit 10)) `sepBy` space1
  , solveA = defSolver
    { solve = Just . sum . fmap (sequenceVal . highestValuedSequence 2)
    }
  , solveB = defSolver
    { solve = Just . sum . fmap (sequenceVal . highestValuedSequence 12)
    }
  , tests =
    [ "987654321111111"
      :=> [(PartA, "98"), (PartB, "987654321111")]
    , "811111111111119"
      :=> [(PartA, "89"), (PartB, "811111111119")]
    , "234234234234278"
      :=> [(PartA, "78"), (PartB, "434234234278")]
    , "818181911112111"
      :=> [(PartA, "92"), (PartB, "888911112111")]
    , "987654321111111 811111111111119 234234234234278 818181911112111"
      :=> [(PartA, "357"), (PartB, "3121910778619")]
    , "2131122323213222222222212132222122121222322522211221222222327123122221422122232222222222222422672222"
      :=> [(PartA, "77")]
    ]
  }

sequenceVal :: [Int] -> Int
sequenceVal = foldl k 0
  where
    k acc x = acc * 10 + x

highestValuedSequence :: Int -> Vector Int -> [Int]
highestValuedSequence 0 _ = []
highestValuedSequence l v = let
  hay = Vector.slice 0 (Vector.length v - l + 1) v
  (x, ix) = findFirstMax hay
  rest = Vector.drop (ix+1) v
  in x : highestValuedSequence (l-1) rest

findFirstMax :: Ord a => Vector a -> (a, Int)
findFirstMax v = Vector.ifoldl cmp z v
  where
    z = (v Vector.! 0, 0)
    cmp acc@(mx, _) i x
      | x > mx = (x, i)
      | otherwise = acc

bruteforceA :: Vector Int -> Int
bruteforceA v = maximum $
  [ v Vector.! a * 10 + v Vector.! b
  | a <- [0 .. length v-2]
  , b <- [a+1 .. length v-1]
  ]

debuggingA :: Vector Int -> Int
debuggingA v = let
  good = bruteforceA v
  questionable = sequenceVal $ highestValuedSequence 2 v
  in if good == questionable
    then good
    else traceShow (good, "bad", questionable, concatMap show v) good