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
    updateMemoryV1 mask address value = HM.insert address (applyMaskV1 mask value)
    (Program _ memory1) = foldl' (executeInstruction updateMemoryV1) p0 instructions
    sol1                = sum memory1
    -- part 2
    (Program _ memory2) = foldl' (executeInstruction updateMemoryV2) p0 instructions
    sol2                = sum memory2
  return (sol1, sol2)

executeInstruction ::
  ([Bitmask] -> Int -> Int -> MemoryMap -> MemoryMap)
  -> Program
  -> Instruction
  -> Program
executeInstruction updateMemory (Program mask memory) instruction =
  case instruction of
    Mask mask'           -> Program mask' memory
    Memory address value -> Program mask . updateMemory mask address value $ memory

applyMaskV1 :: [Bitmask] -> Int -> Int
applyMaskV1 mask = bitsToInt . zipWith maskOneBit mask . intToNBits 36
  where
    maskOneBit Zero     = const False
    maskOneBit One      = const True
    maskOneBit Floating = id

updateMemoryV2 :: [Bitmask] -> Int -> Int -> MemoryMap -> MemoryMap
updateMemoryV2 mask address value =
  HM.union (HM.fromList [(a, value) | a <- applyMaskV2 mask address])

applyMaskV2 :: [Bitmask] -> Int -> [Int]
applyMaskV2 mask = fmap bitsToInt . expandFloating . zip mask . intToNBits 36
  where
    maskOneBit Zero     b = [b]
    maskOneBit One      _ = [True]
    maskOneBit Floating _ = [False, True]
    expandFloating :: [(Bitmask, Bool)] -> [[Bool]]
    expandFloating []           = [[]]
    expandFloating ((m, b):mbs) = [b' : mbs' | mbs' <- expandFloating mbs, b' <- maskOneBit m b]

data Program = Program [Bitmask] MemoryMap

type MemoryMap = HashMap Int Int

data Instruction = Mask [Bitmask] | Memory Int Int
  deriving Show

data Bitmask = Zero | One | Floating
  deriving Show

parseInstruction :: Parser Instruction
parseInstruction = parseMask <|> parseMemory
  where
    parseMask = do
      _  <- string "mask = "
      ms <- many $ choice [Zero <$ char '0', One <$ char '1', Floating <$ char 'X']
      pure $ Mask ms
    parseMemory = do
      _       <- string "mem"
      address <- between (char '[') (char ']') $ signedInt
      _       <- string " = "
      value   <- signedInt
      pure $ Memory address value
