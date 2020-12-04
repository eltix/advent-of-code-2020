module Main where

import           BasicPrelude
import qualified Day01        (computeSolutions)
import qualified Day02        (computeSolutions)
import qualified Day03        (computeSolutions)
import qualified Day04        (computeSolutions)

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
  ]

printSolution :: Show a => IO (a, a) -> IO ()
printSolution solIO = do
  (sol1, sol2) <- solIO
  putStrLn $ "  Part 1: " ++ tshow sol1
  putStrLn $ "  Part 2: " ++ tshow sol2
