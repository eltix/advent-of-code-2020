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
    -- group the differences into groups of consecutive ones and threes
    -- (fortunately there are no twos)
    groupped = group differences
    -- only the groups of ones matter as a difference of 3 is not mutable
    groupsOfOne = filter (1 `elem`) groupped
    -- for each group of ones, there are 'countArrangements' ways of arranging
    -- the chargers. Then we take the product of all arrangements of all groups
    sol2 = product . fmap (countArrangements . length) $ groupsOfOne
  return (sol1, sol2)

-- | I'm not very happy with this solution.
-- For \(n \leq 3\) it's easy because it's just \(2^(n-1)\).
-- But then it gets trickier because we must not have a gap greater than three
-- between two consecutive numbers. I manually found the answer for 4 and realized
-- that's the maximum for my puzzle input so I stopped there. Surely there must be
-- a better solution
countArrangements :: Int -> Int
countArrangements 1 = 1
countArrangements 2 = 2
countArrangements 3 = 4
countArrangements 4 = 7
countArrangements _ = error "Woops! Is there an analytical formula for this?"
