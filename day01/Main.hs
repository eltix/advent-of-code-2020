{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BasicPrelude
import qualified Data.Text as T

main :: IO ()
main = do
  entries <- loadEntries
  let sol = uncurry (*) <$> sumUpTo 2020 entries
  putStrLn $ "Solution to part 1: " ++ tshow sol

sumUpTo :: Int -> [Int] -> Maybe (Int, Int)
sumUpTo target entries = find (\(x, y) -> x + y == target) pairs
  where
    pairs = allPairs entries


-- | This should probably be generalized to k-permutations of any size k
-- in case we need that later
allPairs :: [Int] -> [(Int, Int)]
allPairs [] = []
allPairs (x:xs) = [(x, y) | y <- xs] ++ allPairs xs

loadEntries :: IO [Int]
loadEntries = loadListOfIntsSeparatedByNewLine "inputs/day01/part1.txt"

loadListOfIntsSeparatedByNewLine :: FilePath -> IO [Int]
loadListOfIntsSeparatedByNewLine fp =
  fmap read . T.lines <$> readFile fp
