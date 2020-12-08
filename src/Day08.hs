{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Day08 where

import           BasicPrelude
import           Lens.Micro
import           Lens.Micro.Extras (view)
import           Lens.Micro.TH     (makeLenses)
import           LoadAndParse

data Program =
  Program
  { _code        :: [Instruction]
  , _pointer     :: !Int
  , _accumulator :: !Int
  , _status      :: Status
  } deriving Show

data Instruction = Instruction Operator Int deriving Show

data Operator = Accumulate | Jump | Noop deriving Show

data Status = Running | Halted deriving Show

makeLenses ''Program

computeSolutions :: IO (Maybe Int, Maybe Int)
computeSolutions = do
  instructions <- loadAndParseAsRows parseInstruction "inputs/day08.txt"
  let
    program = Program instructions 0 0 Running
    -- part 1
    sol1 = fromLoop . executeUntilLoop $ program
    -- part 2
    programs = scenarize program
    sol2     = join . find isJust . fmap (fromTerminated . executeUntilLoop) $ programs
  return (sol1, sol2)

data ExecutionResult = TerminatedNormally Int | Loops Int deriving Show

fromLoop :: ExecutionResult -> Maybe Int
fromLoop res = case res of Loops a-> Just a; _ -> Nothing

fromTerminated :: ExecutionResult -> Maybe Int
fromTerminated res = case res of TerminatedNormally a-> Just a; _ -> Nothing

-- | Execute program until the same pointer is reached twice
executeUntilLoop ::
  Program
  -> ExecutionResult
  -- ^ either the program terminates normally or enters a loop
executeUntilLoop = go []
  where
    go pointerHistory program@Program{_pointer, _status} =
      case _status of
        Halted  -> TerminatedNormally . view accumulator $ program
        Running ->
          if _pointer `elem` pointerHistory
            then Loops . view accumulator $ program
            else go (_pointer:pointerHistory) (runOnce program)

-- | Produce different versions of a base program by changing a 'Jump' and 'Noop'
-- or the other way around
scenarize :: Program -> [Program]
scenarize baseProgram@Program{_code} = (\c -> baseProgram & code .~ c) <$> modifiedCodes
  where
    modifiedCodes = catMaybes . zipWith maybeModifyCode [0..] $ _code
    maybeModifyCode :: Int -> Instruction -> Maybe [Instruction]
    maybeModifyCode i (Instruction Jump a) = Just $ _code & ix i .~ Instruction Noop a
    maybeModifyCode i (Instruction Noop a) = Just $ _code & ix i .~ Instruction Jump a
    maybeModifyCode _ _                    = Nothing

-- | Run one instruction of the program
runOnce :: Program -> Program
runOnce p@Program{_code, _pointer}
  | _pointer == length _code = p & status .~ Halted
  | _pointer < length _code  = runInstruction (_code !! _pointer) p
  | otherwise                = error $ "Unexpected pointer value: " ++ show _pointer

runInstruction :: Instruction -> Program -> Program
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
