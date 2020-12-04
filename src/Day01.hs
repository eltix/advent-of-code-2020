module Day01 where

import           BasicPrelude
import qualified Data.Text    as T

computeSolutions :: IO (Maybe Int, Maybe Int)
computeSolutions = do
  entries <- loadEntries
  let
    pairs = arrangements 2 entries
    sol1  = product <$> sumUpTo 2020 pairs
  let
    triples = arrangements 3 entries
    sol2    = product <$> sumUpTo 2020 triples
  return (sol1, sol2)

sumUpTo :: Int -> [[Int]] -> Maybe [Int]
sumUpTo target = find ((== target) . sum)

-- | arrangements is synonymous (albeit quite old-fashioned) with "k-permutations"
-- see https://en.wikipedia.org/wiki/Permutation#k-permutations_of_n
arrangements :: Int -> [a] -> [[a]]
arrangements _ []     = []
arrangements 1 xs     = [[x] | x <- xs]
arrangements n (x:xs) = [x:y | y <- arrangements (n-1) xs] ++ arrangements n xs

loadEntries :: IO [Int]
loadEntries = loadListOfIntsSeparatedByNewLine "inputs/day01.txt"

loadListOfIntsSeparatedByNewLine :: FilePath -> IO [Int]
loadListOfIntsSeparatedByNewLine fp =
  fmap read . T.lines <$> readFile fp
