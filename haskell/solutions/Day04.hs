{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day04 where

import AOC.Solution

import ParsingPrelude
import Grid2D
import Util
import Control.Lens hiding (Empty)

solution :: Solution (Grid2D Tile) Int Int
solution = Solution
  { decodeInput = fromLines <$> (some tileP `sepBy` space1)
  , solveA = defSolver
    { solve = Just . countAccessibleRolls
    }
  , solveB = defSolver
    { solve = Just . removeEventuallyAccessibleRolls
    }
  , tests =
    [ unlines
      [ "..@@.@@@@."
      , "@@@.@.@.@@"
      , "@@@@@.@.@@"
      , "@.@@@@..@."
      , "@@.@@@@.@@"
      , ".@@@@@@@.@"
      , ".@.@.@.@@@"
      , "@.@@@.@@@@"
      , ".@@@@@@@@."
      , "@.@.@@@.@."
      ] :=> [(PartA, "13"), (PartB, "43")]
    ]
  }

data Tile = Roll | Empty deriving (Eq, Show, Ord)

tileP :: Parser Tile
tileP = Roll <$ "@" <|> Empty <$ "."

isRollAccessible :: Grid2D Tile -> Int -> Int -> Bool
isRollAccessible g x y = countHits (==Roll) (adjacents x y g) < 4

countAccessibleRolls :: Grid2D Tile -> Int
countAccessibleRolls g = getSum $ ifoldMap checkTile g
  where
    checkTile _ Empty = 0
    checkTile (V2 x y) Roll = if isRollAccessible g x y then 1 else 0

removeAccessibleRolls :: Grid2D Tile -> (Sum Int, Grid2D Tile)
removeAccessibleRolls g = itraverse clearTile g
  where
    clearTile _ Empty = (0, Empty)
    clearTile (V2 x y) Roll = if isRollAccessible g x y then (1, Empty) else (0, Roll)

removeEventuallyAccessibleRolls :: Grid2D Tile -> Int
removeEventuallyAccessibleRolls = loop 0
  where
    loop removed g = case removeAccessibleRolls g of
      (0, _) -> removed
      (step, g') -> loop (removed + getSum step) g'