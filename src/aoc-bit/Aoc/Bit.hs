module Aoc.Bit
  ( bitsToInt
  , intToNBits
  )
  where

import           BasicPrelude

bitsToInt :: [Bool] -> Int
bitsToInt = sum . fmap (\(i, bit) -> fromEnum bit * 2^i) . zip [(0::Int)..] . reverse

-- | Convert an 'Int' to @n@ bits represented as 'Bool's
intToNBits ::
  Int        -- ^ @n@: size of fixed-size bit list
  -> Int     -- ^ integer to convert
  -> [Bool]  -- ^ list of @n@ bits represented as 'Bool's
intToNBits size = reverse . take size . (++ repeat False) . reverse . toBinary
  where
    toBinary 0 = [False]
    toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2  == 1]
