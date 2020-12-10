module Day10 where

import           BasicPrelude

computeSolutions :: IO (Int, Int)
computeSolutions = do
  numbers :: [Int] <- fmap read . lines <$> readFile "./inputs/day10.txt"
  let
    sorted      = sort numbers
    differences = zipWith (-) (sorted ++ [maximum sorted + 3]) (0:sorted)
    -- part 1
    ones        = length . filter (==1) $ differences
    threes      = length . filter (==3) $ differences
    sol1 = ones * threes
    -- part 2
    groupped = group differences
    groupsOfOne = filter (1 `elem`) groupped
    sol2 = foldl' (\acc x -> acc * (countArrangements . length $ x)) 1 groupsOfOne
  return (sol1, sol2)

countArrangements :: Int -> Int
countArrangements 1 = 1
countArrangements 2 = 2
countArrangements 3 = 4
countArrangements 4 = 7
countArrangements _ = error "woops. Is there an analytical formula for this?"
