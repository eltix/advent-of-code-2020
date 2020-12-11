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
    -- for each group of ones of size n, there are 'validConfigurations (n-1)' ways of arranging
    -- the chargers. Then we take the product of all configurations of all groups
    sol2 = product . fmap (validConfigurations . (subtract 1) . length) $ groupsOfOne
  return (sol1, sol2)

-- | The number of possible binary configurations in a list of \(n\) bits such that
-- the maximum number of consecutive zeros is 2. For \(n \leq 2\) it's easy because it's just \(2^n\).
-- Then we can define it recursively for any \(n\) as the sum of the last three terms of the sequence.
-- Consider a sequence of \(n\) bits. We can either:
--
--   * set the first bit to 1 and then we have validConfigurations (n-1) configurations
--   * set the first bit to 0 and the second to 1 and then we have validConfigurations (n-2) configurations
--   * set the first two bits to 0 and the third to 1 and then we have validConfigurations (n-3) configurations
--   * and that's it! we can't set more than two bits to 0
--
-- Credit goes to malcomar for the inductive reasoning
validConfigurations :: Int -> Int
validConfigurations 0 = 1
validConfigurations 1 = 2
validConfigurations 2 = 4
validConfigurations n = validConfigurations (n-1) + validConfigurations (n-2) + validConfigurations (n-3)
