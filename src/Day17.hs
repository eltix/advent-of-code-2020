{-# LANGUAGE FlexibleInstances #-}

module Day17 where

import           BasicPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set

import           LoadAndParse

computeSolutions :: IO (Int, Int)
computeSolutions = do
  initialLayer :: [[Cube]] <- loadAndParseAsRows (many parseCube) "inputs/day17.txt"
  let
    -- part 1
    grid3d         = initializeGrid initialLayer :: Grid Coord3d
    sol1           = numActiveAfterSixCycles grid3d
    -- part 2
    grid4d         = initializeGrid initialLayer :: Grid Coord4d
    sol2           = numActiveAfterSixCycles grid4d
  return (sol1, sol2)

numActiveAfterSixCycles :: (Coord a, Ord a, Hashable a) => Grid a -> Int
numActiveAfterSixCycles grid = HM.size . HM.filter (== Active) $ sixthCycleGrid
  where sixthCycleGrid = (!! 6) . iterate oneCycle $ grid

oneCycle :: (Coord a, Ord a, Hashable a) => Grid a -> Grid a
oneCycle grid = HM.union (HM.fromList $ zip coordinates newStates) grid
  where
    coordinates = Set.toList $ neighborsOfActive grid `Set.union` (Set.fromList $ HM.keys grid)
    newStates   = applyRule <$> coordinates
    applyRule position
      | cube == Just Active && numNeighborsActive `elem` [2, 3] = Active
      | cube == Just Active                                     = Inactive
      | numNeighborsActive == 3                                 = Active
      | otherwise                                               = Inactive
      where
        cube = HM.lookup position grid
        neighbors = neighborsOf position
        numNeighborsActive =
          length . filter (== Just Active) . fmap (`HM.lookup` grid) $ neighbors

neighborsOfActive :: (Coord a, Ord a) => Grid a -> Set a
neighborsOfActive grid =
  Set.fromList . concatMap neighborsOf . HM.keys . HM.filter (== Active) $ grid


data Cube = Active | Inactive
  deriving (Show, Eq)

parseCube :: Parser Cube
parseCube = choice [Active <$ char '#', Inactive <$ char '.']

type Grid a = HashMap a Cube

class Coord a
  where
    lift2d      :: (Int, Int) -> a
    neighborsOf :: a -> [a]

type Coord3d = (Int, Int, Int)
instance Coord Coord3d
  where
    lift2d (x, y) = (x, y, 0)
    neighborsOf (x, y, z) =
      [ (x+i, y+j, z+k)
      | i <- [-1..1], j <- [-1..1], k <- [-1..1]
      , (i, j, k) /= (0, 0, 0)
      ]

type Coord4d = (Int, Int, Int, Int)
instance Coord Coord4d
  where
    lift2d (x, y) = (x, y, 0, 0)
    neighborsOf (x, y, z, t) =
      [ (x+i, y+j, z+k, t+l)
      | i <- [-1..1], j <- [-1..1], k <- [-1..1], l <- [-1..1]
      , (i, j, k, l) /= (0, 0, 0, 0)
      ]

initializeGrid :: (Coord a, Eq a, Hashable a) => [[Cube]] -> Grid a
initializeGrid = HM.fromList . concat . zipWith toColumn [0..] . fmap toRow
  where
    toRow :: [Cube] -> [(Int, Cube)]
    toRow = zip [0..]
    toColumn :: forall a. Coord a => Int -> [(Int, Cube)] -> [(a, Cube)]
    toColumn rowIndex xs = [(lift2d (rowIndex, colIndex), cube) | (colIndex, cube) <- xs]
