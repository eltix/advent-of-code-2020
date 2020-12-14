module Day14 where

import           BasicPrelude
import qualified Data.HashMap.Strict as HM

import           Aoc.Bit             (bitsToInt, intToNBits)
import           LoadAndParse

computeSolutions :: IO (Int, Int)
computeSolutions = do
  instructions <- loadAndParseAsRows parseInstruction "inputs/day14.txt"
  let
    -- A program without any mask is undefined. The first instruction has to be
    -- setting a new mask
    p0 = Program undefined mempty
    -- part 1
    (Program _ memory) = foldl' executeInstruction p0 instructions
    sol1               = sum memory
    -- part 2
    sol2 = 0
  return (sol1, sol2)

executeInstruction :: Program -> Instruction -> Program
executeInstruction (Program mask memory) instruction =
  case instruction of
    Mask mask'           -> Program mask' memory
    Memory address value -> Program mask $ HM.insert address (maskValue mask value) memory

maskValue :: [Bitmask] -> Int -> Int
maskValue mask = bitsToInt . zipWith applyMask mask . intToNBits 36
  where
    applyMask Zero = const False
    applyMask One  = const True
    applyMask Nil  = id

data Program = Program [Bitmask] MemoryMap

type MemoryMap = HashMap Int Int

data Instruction = Mask [Bitmask] | Memory Int Int
  deriving Show

data Bitmask = Zero | One | Nil
  deriving Show

parseInstruction :: Parser Instruction
parseInstruction = parseMask <|> parseMemory
  where
    parseMask = do
      _  <- string "mask = "
      ms <- many $ choice [Zero <$ char '0', One <$ char '1', Nil <$ char 'X']
      pure $ Mask ms
    parseMemory = do
      _       <- string "mem"
      address <- between (char '[') (char ']') $ signedInt
      _       <- string " = "
      value   <- signedInt
      pure $ Memory address value
