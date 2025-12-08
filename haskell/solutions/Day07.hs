{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day07 where

import AOC.Solution

import ParsingPrelude
import Util hiding (splits)
import Grid2D
import qualified Data.IntSet as IntSet
import Control.Lens hiding (Empty)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

solution :: Solution (Grid2D Tile) Int Int
solution = Solution
  { decodeInput = fromLines <$> (some tileP `sepBy` space1)
  , solveA = defSolver
    { solve = Just . countBeamSplits
    }
  , solveB = defSolver
    { solve = Just . countTimelines
    }
  , tests =
    [ unlines 
      [ ".......S......."
      , "..............."
      , ".......^......."
      , "..............."
      , "......^.^......"
      , "..............."
      , ".....^.^.^....."
      , "..............."
      , "....^.^...^...."
      , "..............."
      , "...^.^...^.^..."
      , "..............."
      , "..^...^.....^.."
      , "..............."
      , ".^.^.^.^.^...^."
      , "..............."
      ] :=> [(PartA, "21"), (PartB, "40")]
    ]
  }

data Tile = Start | Empty | Split
  deriving (Eq, Ord, Show)

tileP :: Parser Tile
tileP = asum [Start <$ "S", Empty <$ ".", Split <$ "^"]

countBeamSplits :: Grid2D Tile -> Int
countBeamSplits = getSum . fst . foldl' step (0, IntSet.empty) . rows
  where
    step (splits, beams) row = let
      inspect i = \case
        Start -> (0, IntSet.singleton i)
        Empty | beams ^. contains i
          -> (0, IntSet.singleton i)
        Split | beams ^. contains i
          -> (1, IntSet.fromList [i-1, i+1])
        _ -> mempty
      (splitsHere, newBeam) = ifoldMap inspect row
      in (splits + splitsHere, newBeam)

countTimelines :: Grid2D Tile -> Int
countTimelines = sum . foldl' step IntMap.empty . rows
  where
    step bundle row = let
      inspect :: Int -> Tile -> IntMap Int -> IntMap Int
      inspect i = \case
        Start -> IntMap.insert i 1
        Split | Just prev <- IntMap.lookup i bundle
          -> IntMap.insertWith (+) (i-1) prev
           . IntMap.insertWith (+) (i+1) prev
           . IntMap.delete i
        _ -> id
      in ifoldr' inspect bundle row