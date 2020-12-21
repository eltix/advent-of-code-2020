module Day15 where

import           BasicPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence       as S

-- import Debug.Trace

computeSolutions :: IO (Int, Int)
computeSolutions =
  let
    numbers = [6,4,12,1,20,0,16]
    -- part 1
    sol1 = spokenSeq 2020 numbers
    -- part 2
    sol2 = spokenSeq 30000000 numbers
  in pure (sol1, sol2)

spokenSeq :: Int -> [Int] -> Int
spokenSeq numTurns starting = snd . foldr oneTurn (s0, last starting) $ turns
  where
    turns = [numTurns,(numTurns-1)..(length starting + 1)]

    s0 :: S.Seq Int
    s0 = let f (n, turn) = S.update n turn
      in foldr f (S.replicate (numTurns+1) 0) $ zip (init starting) [1..]

    oneTurn :: Int -> (S.Seq Int, Int) -> (S.Seq Int, Int)
    oneTurn turn (s, n) = case s `S.index` n of
      0              -> (S.update n (turn-1) s, 0)
      lastTurnSpoken -> (S.update n (turn-1) s, turn-1-lastTurnSpoken)


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
