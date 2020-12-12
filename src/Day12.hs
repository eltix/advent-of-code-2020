{-# LANGUAGE LambdaCase #-}

module Day12 where

import           BasicPrelude hiding (Either (..))
import           Lens.Micro

import           LoadAndParse

computeSolutions :: IO (Int, Int)
computeSolutions = do
  instructions <- loadAndParseAsRows parseInstruction "inputs/day12.txt"
  let
    origin = (0, 0)
    state0 = (origin, East)
    -- part 1
    lastState = foldl' move state0 instructions
    sol1      = manhattan origin $ fst lastState
    -- part 2
    sol2 = 0
  return (sol1, sol2)

move :: ((Int, Int), Cardinal) -> Instruction -> ((Int, Int), Cardinal)
move s@(_, orientation) = \case
  Translation card n -> s & _1 %~ translate card n
  Rotation dir qts   -> s & _2 %~ rotate dir qts
  Forward n          -> s & _1 %~ translate orientation n

translate :: Cardinal -> Int -> (Int, Int) -> (Int, Int)
translate North n = over _2 (+n)
translate South n = over _2 (subtract n)
translate East  n = over _1 (+n)
translate West  n = over _1 (subtract n)

rotate :: Direction -> Int -> Cardinal -> Cardinal
rotate Right n = toEnum . (`mod` 4) . (+n)         . fromEnum
rotate Left  n = toEnum . (`mod` 4) . (subtract n) . fromEnum


data Instruction =
  Translation Cardinal Int
  | Rotation Direction Int
  | Forward Int
  deriving Show

-- | Sorted in clock-wise order
data Cardinal = North | East | South | West deriving (Show, Enum)
data Direction = Left | Right deriving Show

parseInstruction :: Parser Instruction
parseInstruction = parseTranslation <|> parseRotation <|> parseForward
  where
    parseTranslation = do
      cardinal <- choice [North <$ char 'N', South <$ char 'S', East <$ char 'E', West <$ char 'W']
      n        <- signedInt
      pure $ Translation cardinal n
    parseRotation = do
      dir <- choice [Left <$ char 'L', Right <$ char 'R']
      qt  <- choice [1 <$ string "90", 2 <$ string "180", 3 <$ string "270"]
      pure $ Rotation dir qt
    parseForward = do
      _ <- char 'F'
      n <- signedInt
      pure $ Forward n

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (xa, ya) (xb, yb) = abs (xb-xa) + abs (yb-ya)
