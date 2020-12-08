{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Day08 where

import           BasicPrelude
import           Lens.Micro
import           Lens.Micro.TH (makeLenses)

import           LoadAndParse

data Program =
  Program
  { _code        :: [Instruction]
  , _pointer     :: Int
  , _accumulator :: Int
  } deriving Show

data Instruction = Instruction Operator Int
  deriving Show

data Operator = Accumulate | Jump | Noop
  deriving Show

makeLenses ''Program

computeSolutions :: IO (Int, Int)
computeSolutions = do
  instructions <- loadAndParseAsRows parseInstruction "inputs/day08.txt"
  let
    program = Program instructions 0 0
    -- part 1
    programBeforeLoop = executeUntilLoop program
    sol1              = programBeforeLoop ^. accumulator
    -- part 2
    sol2     = 0
  return (sol1, sol2)

-- | Execute program until the same pointer is reached twice
executeUntilLoop :: Program -> Program
executeUntilLoop = go []
  where
    go pointerHistory program@Program{_pointer} =
      if _pointer `elem` pointerHistory
        then program
        else go (_pointer:pointerHistory) (runOnce program)

-- | Run one instruction of the program
runOnce :: Program -> Program
runOnce p@Program{_code, _pointer} = runInstruction (_code !! _pointer) p
  where
    runInstruction (Instruction Accumulate a) = over accumulator (+a) . over pointer (+1)
    runInstruction (Instruction Jump j)       = over pointer (+j)
    runInstruction (Instruction Noop _)       = over pointer (+1)

parseInstruction :: Parser Instruction
parseInstruction = do
  op  <- parseOperator
  arg <- lexeme signedInt <?> "integer"
  pure $ Instruction op arg

parseOperator :: Parser Operator
parseOperator = lexeme . choice $
  [ Accumulate <$ symbol "acc"
  , Jump       <$ symbol "jmp"
  , Noop       <$ symbol "nop"
  ]
