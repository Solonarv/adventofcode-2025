{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}
module Day06 where

import AOC.Solution

import ParsingPrelude
import Util
import Data.List.Split (splitWhen)
import Data.Char (isSpace)

solution :: Solution ProblemSet Int Int
solution = Solution
  { decodeInput = do
      let lineP = some (oneOf @[] "0123456789 ")
          opP = asum [Plus <$ "+", Times <$ "*"]
      inputs <- lineP `sepEndBy1` "\n"
      operations <- opP `sepBy1` noeol
      pure ProblemSet{..}
  , solveA = defSolver
    { solve = Just . sum . problemSetAnswers
    }
  , solveB = defSolver
    { solve = Just . sum . cephalopodAnswers
    }
  , tests =
    [ unlines
      [ "123 328  51 64 "
      , " 45 64  387 23 "
      , "  6 98  215 314"
      , "*   +   *   +  "
      ] :=> [(PartA, "4277556"), (PartB, "3263827")]
    ]
  }

data ProblemSet = ProblemSet
  { inputs :: [String]
  , operations :: [Op]
  }
  deriving (Eq, Ord, Show)

data Op = Plus | Times
  deriving (Eq, Ord, Show)

operate :: Op -> [Int] -> Int
operate = \case Plus{} -> sum ; Times{} -> product

problemSetAnswers :: ProblemSet -> [Int]
problemSetAnswers ProblemSet{..} = zipWith operate operations groups
  where groups = transpose (fmap read . words <$> inputs)

cephalopodAnswers :: ProblemSet -> [Int]
cephalopodAnswers ProblemSet{..} = let
    inputCols = transpose inputs
    rawGroups = splitWhen (all (==' ')) inputCols
    toNumbers = read @Int . dropWhileEnd isSpace . dropWhile isSpace
    groups = fmap toNumbers <$> rawGroups
  in zipWith operate operations groups