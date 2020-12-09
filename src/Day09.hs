module Day09 where

import           BasicPrelude

import           Day01        (arrangements, sumUpTo)

computeSolutions :: IO (Maybe Int, Maybe Int)
computeSolutions = do
  numbers :: [Int] <- fmap read . lines <$> readFile "./inputs/day09.txt"
  let
    xmasPreamble = 25
    -- part 1
    invalidIndex = find (not . isValid numbers xmasPreamble) [xmasPreamble..(length numbers)-1]
    sol1         = (numbers !!) <$> invalidIndex
    -- part 2
    -- use the fact that Maybe is a Monad to manipulate its content more conveniently
    sol2 = do
      target <- sol1
      -- only numbers less that target can hope to sum up to target
      let candidates = filter (< target) numbers
      consecutives <- findConsecutiveSumUpTo target candidates
      pure $ minimum consecutives + maximum consecutives
  return (sol1, sol2)

isValid :: [Int] -> Int -> Int -> Bool
isValid numbers preambleSize index = isJust $ sumUpTo n (arrangements 2 previous)
  where
    n        = numbers !! index
    previous = [numbers !! i | i <- [(index-preambleSize)..(index-1)]]

findConsecutiveSumUpTo :: Int -> [Int] -> Maybe [Int]
findConsecutiveSumUpTo target numbers =
  let subLists = flip drop numbers <$> [0..(length numbers - 2)]
  in join . find isJust . fmap (consecutiveSumUpTo target) $ subLists

consecutiveSumUpTo :: Int -> [Int] -> Maybe [Int]
consecutiveSumUpTo target numbers = go [] 0
  where
    go acc i
      | i == length numbers = Nothing
      | sum acc > target    = Nothing
      | otherwise           =
        if sum acc + numbers !! i == target
          then Just $ (numbers !! i) : acc
          else go ((numbers !! i) : acc) (i+1)


-- | Example from https://adventofcode.com/2020/day/9
-- >>> (example !!) <$> find (not . isValid example 5) [5..(length example)-1]
-- Just 127
example :: [Int]
example = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]
