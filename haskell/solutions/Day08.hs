{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day08 where

import AOC.Solution

import ParsingPrelude
import Data.Vector (Vector)
import Linear.V3
import qualified Data.Vector as Vector
import Control.Lens
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Util hiding (insert)
import DynMap
import Control.Monad.ST (runST)
import qualified Data.Vector.Algorithms.Intro as IntroSort
import Data.Ord

solution :: Solution (Vector (V3 Int)) Int Int
solution = Solution
  { decodeInput = Vector.fromList <$> (v3P `sepBy` space1)
  , solveA = defSolver
    { solve = Just . product . take 3 . sortOn negate . fmap Set.size . cliques . shortestConnections (getDyn "count" 1000)
    }
  , solveB = defSolver
    { solve = fmap checksum . lastRequiredConnection
    }
  , tests =
    [ WithDyn @Int "count" 10 $
      unlines
      [ "162,817,812"
      , "57,618,57"
      , "906,360,560"
      , "592,479,940"
      , "352,342,300"
      , "466,668,158"
      , "542,29,236"
      , "431,825,988"
      , "739,650,466"
      , "52,470,668"
      , "216,146,977"
      , "819,987,18"
      , "117,168,530"
      , "805,96,715"
      , "346,949,466"
      , "970,615,88"
      , "941,993,340"
      , "862,61,35"
      , "984,92,344"
      , "425,690,689"
      ] :=> [(PartA, "40"), (PartB, "25272")]
    ]
  }

v3P :: Parser (V3 Int)
v3P = V3
  <$> do decimal <* ","
  <*> do decimal <* ","
  <*> decimal

data Connection = !(V3 Int) :--: !(V3 Int)
  deriving (Eq, Ord, Show)

metric :: Connection -> Int
metric (x :--: y) = sum . fmap (^2) $ x - y

pairs :: Vector (V3 Int) -> Vector Connection
pairs v = flip ifoldMap v \i here -> let
  before = Vector.slice 0 i v
  in (here :--:) <$> before

-- a priority queue that knows its own size
data SizedPQueue v = SizedPQueue !Int !(IntMap [v])
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

-- if the input is already larger than maxSize it is only guaranteed
-- that it will not grow
insert :: Int -> Int -> v -> SizedPQueue v -> SizedPQueue v
insert maxSize k v (SizedPQueue sz q)
  | sz < maxSize = SizedPQueue (sz+1) (IntMap.insertWith (++) k [v] q)
  | otherwise = SizedPQueue maxSize (trimPQ $ IntMap.insertWith (++) k [v] q)
  where
    trimPQ = IntMap.updateMax \case
      [] -> Nothing
      [_] -> Nothing
      xs -> Just (drop 1 xs)

emptySizedPQueue :: SizedPQueue v
emptySizedPQueue = SizedPQueue 0 IntMap.empty

shortestConnections :: Int -> Vector (V3 Int) -> SizedPQueue Connection
shortestConnections n = foldl' step emptySizedPQueue . pairs
  where
    step queue conn = insert n (metric conn) conn queue

cliques :: Foldable f => f Connection -> [Set (V3 Int)]
cliques = foldl updateCliques []

updateCliques :: [Set (V3 Int)] -> Connection -> [Set (V3 Int)]
updateCliques [] (x :--: y) = [Set.fromList [x, y]]
updateCliques soc@(c:cs) conn@(x :--: y)
  | hasX && hasY = soc
  | x `Set.member` c = mergeClique (Set.insert y c) y cs
  | y `Set.member` c = mergeClique (Set.insert x c) x cs
  | otherwise = c : updateCliques cs conn
  where
    hasX = x `Set.member` c; hasY = y `Set.member` c

mergeClique :: Ord t => Set t -> t -> [Set t] -> [Set t]
mergeClique payload _ [] = [payload]
mergeClique payload target (c:cs)
  | target `Set.member` c = (payload <> c) : cs
  | otherwise = c : mergeClique payload target cs

lastRequiredConnection :: Vector (V3 Int) -> Maybe Connection
lastRequiredConnection points = loop [] sorted
  where
    sorted = runST do
      mv <- Vector.thaw (pairs points)
      IntroSort.sortBy (comparing metric) mv
      Vector.unsafeFreeze mv
    loop cs ps 
      | Vector.null ps = Nothing
      | [Set.size -> sz] <- cliques'
      , sz == Vector.length points
      = Just conn
      | otherwise = loop cliques' (Vector.drop 1 ps)
      where
        conn = ps Vector.! 0
        cliques' = updateCliques cs conn

checksum :: Connection -> Int
checksum (V3 x1 _ _ :--: V3 x2 _ _ ) = x1 * x2