module Day05 where

import           BasicPrelude

import           LoadAndParse

data Seat = Seat{row :: Int, column :: Int}
  deriving (Show)

parseSeat :: Parser Seat
parseSeat = do
  fbs <- bitsToInt <$> count 7 parseBF
  lrs <- bitsToInt <$> count 3 parseRL
  pure $ Seat fbs lrs
  where
    parseBF = choice [True <$ char 'B', False <$ char 'F']
    parseRL = choice [True <$ char 'R', False <$ char 'L']

bitsToInt :: [Bool] -> Int
bitsToInt bits = sum $ [fromEnum b * 2^i | (i, b) <- zip [0::Int ..] (reverse bits)]

toId :: Seat -> Int
toId (Seat r c) = 8*r + c

main :: IO ()
main = do
  seats <- loadAndParseAsRows parseSeat "inputs/day05.txt"
  let
    ids = toId <$> seats
    sol1 = maximum ids
  print sol1
