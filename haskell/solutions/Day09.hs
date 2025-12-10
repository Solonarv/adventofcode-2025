{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day09 where

import AOC.Solution

import ParsingPrelude
import Util
import Linear.V2
import Data.IntervalMap.Strict (IntervalMap)
import Data.Interval (Extended (..), (<=..<), Interval, Boundary (..))
import qualified Data.IntervalMap.Strict as IntervalMap
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IntervalSet
import Data.Coerce
import qualified Data.Interval as Interval
import Grid2D (genGrid, showCharGrid)
import Data.Bool

solution :: Solution [V2 Int] Int Int
solution = Solution
  { decodeInput = liftA2 V2 decimal ("," *> decimal) `sepBy` space1
  , solveA = defSolver
    { solve = Just . largestRectangle
    }
  , solveB = defSolver
    { solve = Just . largestRectangleInLoop
    }
  , tests =
    [ "7,1 11,1 11,7 9,7 9,5 2,5 2,3 7,3"
      :=> [(PartA, "50"), (PartB, "24")]
    ]
  }

largestRectangle :: [V2 Int] -> Int
largestRectangle pts = maximum
  [ area a b
  | (a:bs) <- tails pts
  , b <- bs
  ]

area :: V2 Int -> V2 Int -> Int
area (V2 x y) (V2 x' y') = (abs (x-x')+1)*(abs (y-y')+1)

largestRectangleInLoop :: [V2 Int] -> Int
largestRectangleInLoop pts = maximum
  [ area a b
  | (a:bs) <- tails pts
  , b <- bs
  , inArea loopArea a b
  ]
  where
    loopArea = let ar = enclosedArea pts in trace (pictureArea 15 15 ar) ar

newtype Area = Area (IntervalMap Int (IntervalSet Int))

instance Semigroup Area where
  (<>) = coerce $ IntervalMap.unionWith @Int (IntervalSet.union @Int)

instance Monoid Area where
  mempty = Area $ IntervalMap.singleton (0 <=..< PosInf) IntervalSet.empty

inArea :: Area -> V2 Int -> V2 Int -> Bool
inArea (Area ar) (V2 x1 y1) (V2 x2 y2) = let
  xi = x1 `finInt` x2
  yi = IntervalSet.singleton $ y1 `finInt` y2
  (_, cols, _) = IntervalMap.split xi ar
  in and
    [ yi `IntervalSet.isSubsetOf` col
    | (_, col) <- IntervalMap.toList cols
    ]

enclosedArea :: [V2 Int] -> Area
enclosedArea ps = let
  pairs = traceShowId $ zip ps (drop 1 $ cycle ps)
  in foldl processPair mempty pairs

processPair :: Area -> (V2 Int, V2 Int) -> Area
processPair ar (V2 x1 y1, V2 x2 y2)
  | x1 == x2 = toggleHStrip x1 (min y1 y2) (max y1 y2) ar
  | y1 == y2 = toggleVStrip y1 (min x1 x2) (max x1 x2) ar
  | otherwise = error "processPair: malformed input"

toggleVStrip, toggleHStrip :: Int -> Int -> Int -> Area -> Area
toggleHStrip x y_ y' = toggleRect (Finite x <=..< PosInf) (y_ `finInt` y')
toggleVStrip y x_ x' = toggleRect (x_ `finInt` x') (Finite y <=..< PosInf)

toggleRect :: Interval Int -> Interval Int -> Area -> Area
toggleRect xi yi (Area ar) = Area let
  (before, here, after) = IntervalMap.split xi ar
  yis = IntervalSet.singleton yi
  toggle col = let
      outside = col `IntervalSet.difference` yis
      inside = col `IntervalSet.intersection` yis
    in outside <> IntervalSet.complement inside
  in before <> (toggle <$> here) <> after

finInt :: Int -> Int -> Interval Int
finInt a b = lo <=..< hi
  where
    lo = Finite $ min a b
    hi = Finite $ 1 + max a b

-- section of finInt
finBounds :: Interval Int -> (Int, Int)
finBounds iv = case (Interval.lowerBound' iv, Interval.upperBound' iv) of
  ((Finite x_, Closed), (Finite x', Open)) -> (x_, x'-1)
  _ -> error $ "finBounds: not a [x, y) interval: " ++ show iv

pictureArea :: Int -> Int -> Area -> String
pictureArea w h (Area ar) 
  | null ar = "<empty>"
  | otherwise
  = let
  gr = genGrid w h \x y -> case IntervalMap.lookup x ar of
      Nothing -> False
      Just col -> IntervalSet.member y col
  in showCharGrid (bool '.' 'O') gr