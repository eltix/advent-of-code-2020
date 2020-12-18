{-# LANGUAGE BangPatterns #-}

module Day15 where

import           BasicPrelude
import qualified Data.HashMap.Strict as HM
-- import Debug.Trace
-- import Control.DeepSeq

computeSolutions :: IO (Int, Int)
computeSolutions =
  let
    numbers = [6,4,12,1,20,0,16]
    -- part 1
    sol1 = 0 -- spoken 2020 numbers
    -- part 2
    sol2 = 0 -- spoken 300000000 numbers
  in do
    pure (sol1, sol2)

spoken :: Int -> [Int] -> Int
spoken numTurns starting = go initialMap (last starting) (length starting + 1)
  where
    initialMap :: HashMap Int Int
    initialMap = HM.fromList $ zip (init starting) [1..]
    go m n turn
      | turn == numTurns    = n'
      | otherwise           = go m' n' (turn+1)
      where
        (m', n') = f m n turn


    f :: HashMap Int Int -> Int -> Int -> (HashMap Int Int, Int)
    f m n turn = case HM.lookup n m of
      Nothing             -> (HM.insert n (turn-1) m, 0)
      Just lastTurnSpoken -> (HM.insert n (turn-1) m, turn-1-lastTurnSpoken)
