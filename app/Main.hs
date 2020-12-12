module Main where

import           BasicPrelude
import qualified Day01        (computeSolutions)
import qualified Day02        (computeSolutions)
import qualified Day03        (computeSolutions)
import qualified Day04        (computeSolutions)
import qualified Day05        (computeSolutions)
import qualified Day06        (computeSolutions)
import qualified Day07        (computeSolutions)
import qualified Day08        (computeSolutions)
import qualified Day09        (computeSolutions)
import qualified Day10        (computeSolutions)
import qualified Day11        (computeSolutions)
import qualified Day12        (computeSolutions)

main :: IO ()
main = do
  day <- parseDay <$> getArgs
  putStrLn $ "Solutions to day " ++ tshow day
  solutions !! (day - 1)

parseDay :: [Text] -> Int
parseDay []    = 1
parseDay (s:_) = read s

solutions :: [IO ()]
solutions =
  [ printSolution Day01.computeSolutions
  , printSolution Day02.computeSolutions
  , printSolution Day03.computeSolutions
  , printSolution Day04.computeSolutions
  , printSolution Day05.computeSolutions
  , printSolution Day06.computeSolutions
  , printSolution Day07.computeSolutions
  , printSolution Day08.computeSolutions
  , printSolution Day09.computeSolutions
  , printSolution Day10.computeSolutions
  , printSolution Day11.computeSolutions
  , printSolution Day12.computeSolutions
  ]

printSolution :: (Show a, Show b) => IO (a, b) -> IO ()
printSolution solIO = do
  (sol1, sol2) <- solIO
  putStrLn $ "  Part 1: " ++ tshow sol1
  putStrLn $ "  Part 2: " ++ tshow sol2
