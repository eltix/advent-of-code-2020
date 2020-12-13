module Day13 where

import           BasicPrelude
import           Data.Maybe   (fromJust)
import qualified Data.Text    as T

computeSolutions :: IO (Int, Int)
computeSolutions = do
  raw :: [Text] <- lines <$> readFile "./inputs/day13.txt"
  let
    depart = read $ head raw :: Int
    buses = mapMaybe readBus . T.splitOn "," . last $ raw :: [Int]
    readBus t
      | t == "x" = Nothing
      | otherwise = Just . read $ t
    (busId, wait) =
      minimumBy (compare `on` snd) . fmap (\x -> (x, x - depart `rem` x)) $ buses :: (Int, Int)
    -- part 1
    sol1 = busId * wait
    -- part 2
    busesWithDelay = sortOn snd . mapMaybe readBusWithDelay . zip [0,(-1)..] . T.splitOn "," . last $ raw :: [(Int, Int)]
    readBusWithDelay (i, t)
      | t == "x" = Nothing
      | otherwise = Just (i, read t)
    sol2 = h busesWithDelay
  return (sol1, sol2)

h :: [(Int, Int)] -> Int
h ((a, x):(b, y):[]) = x*u + a
  where (u, _) = diophante (a, x) (b, y)
h xs =
  let
    (a, x) = head xs
    ys = (\(b, y) -> (fst $ diophante (a, x) (b, y), y)) <$> tail xs
  in x * (h ys) + a


-- | Find the first solution \((u, v)\) to the equation \(ux + a = vy + b\)
--
-- The list of candidates is infinite but since x and y are prime in our case, Bezout's identity
-- guarantees that the existence of a solution (see https://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity)
diophante :: (Int, Int) -> (Int, Int) -> (Int,Int)
diophante (a, x) (b, y) = fromJust $
  find (\(u, v) -> u*x + a == v*y + b)
  [(i, j) | i<- [1..], j<-[1..i]]
