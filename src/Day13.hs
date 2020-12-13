module Day13 where

import           BasicPrelude
import           Data.Maybe   (fromJust)
import qualified Data.Text    as T

computeSolutions :: IO (Int, Int)
computeSolutions = do
  input :: [Text] <- lines <$> readFile "./inputs/day13.txt"
  let
    depart          = read $ head input :: Int
    busesWithDelays = parseBuses $ last input
    -- part 1
    (busId, wait) =
      minimumBy (compare `on` snd) . fmap (\(_, x) -> (x, x - depart `rem` x)) $ busesWithDelays
    sol1 = busId * wait
    -- part 2
    sol2 = findEarliest busesWithDelays
  return (sol1, sol2)

-- | FIXME: make this function understandable. It's a mess
findEarliest :: [(Int, Int)] -> Int
findEarliest []                 = 0 -- degenerate case
findEarliest (_:[])             = 0 -- degenerate case
findEarliest ((a, x):(b, y):[]) =
  let (u, _) = diophante (a, x) (b, y)
  in x*u + a
findEarliest ((a, x):xs)        =
  let ys = (\(b, y) -> (fst $ diophante (a, x) (b, y), y)) <$> xs
  in x * (findEarliest ys) + a

-- | Find the first solution \((u, v)\) to the equation \(ux + a = vy + b\)
--
-- The list of candidates is infinite but since x and y are prime in our case, Bezout's identity
-- guarantees the existence of a solution (see https://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity)
-- FIXME: add a maximum iteration parameter and return a @Maybe (Int, Int)@ instead
diophante :: (Int, Int) -> (Int, Int) -> (Int,Int)
diophante (a, x) (b, y) = fromJust $
  find (\(u, v) -> u*x + a == v*y + b)
  [(i, j) | i<- [1..], j<-[1..i]]

parseBuses :: Text -> [(Int, Int)]
parseBuses =
  sortOn snd . mapMaybe readBusWithDelay . zip [0,(-1)..] . T.splitOn ","
  where
    readBusWithDelay (i, t)
      | t == "x"  = Nothing
      | otherwise = Just (i, read t)
