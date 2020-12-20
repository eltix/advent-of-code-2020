module Day15 where

import           BasicPrelude
import qualified Data.HashMap.Strict as HM

computeSolutions :: IO (Int, Int)
computeSolutions =
  let
    numbers = [6,4,12,1,20,0,16]
    -- part 1
    sol1 = spoken 2020 numbers
    -- part 2
    -- XXX: disabled because takes too long (around 60s)
    sol2 = 0 -- spoken 30000000 numbers
  in do
    pure (sol1, sol2)

spoken :: Int -> [Int] -> Int
spoken numTurns starting = snd . foldr oneTurn (m0, last starting) $ turns
  where
    turns = [numTurns,(numTurns-1)..(length starting + 1)]
    m0 :: HashMap Int Int
    m0 = HM.fromList $ zip (init starting) [1..]
    oneTurn :: Int -> (HashMap Int Int, Int) -> (HashMap Int Int, Int)
    oneTurn turn (m, n) = case HM.lookup n m of
      Nothing             -> (HM.insert n (turn-1) m, 0)
      Just lastTurnSpoken -> (HM.insert n (turn-1) m, turn-1-lastTurnSpoken)
