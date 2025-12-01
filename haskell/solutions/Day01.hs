{-# LANGUAGE OverloadedStrings #-}
module Day01 where

import AOC.Solution

import ParsingPrelude
import Util hiding (invert)
import Data.Finite

solution :: Solution [Integer] Int Integer
solution = Solution
  { decodeInput = parseInstruction `sepBy` space1
  , solveA = defSolver
    { solve = Just . countHits (==0) . scanl (+) 50 . map (modulo @100)
    }
  , solveB = defSolver
    { solve = Just . zeroPasses 100 50
    }
  , tests =
    [ "L68 L30 R48 L5 R60 L55 L1 L99 R14 L82"
      :=> [(PartA, "3"), (PartB, "6")]
    , "R1000 L700" :=> [(PartB, "17")]
    ]
  }

parseInstruction :: Parser Integer
parseInstruction = ((negate <$ "L") <|> (id <$ "R")) <*> decimal

zeroPasses :: Integer -> Integer -> [Integer] -> Integer
zeroPasses size start = fst . foldl step (0, start)
  where
    invert x | x == 0 = 0 | otherwise = size-x
    step (acc, pos) i = let
      pos' = (i + pos) `mod` size
      passes
        | i >= 0 = (i + pos) `div` size
        | otherwise = (invert pos - i) `div` size
      in -- traceShow ((acc, pos), i, pos', passes)
        (acc + passes, pos')