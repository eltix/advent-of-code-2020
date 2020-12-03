
module Main where

import           BasicPrelude
import           Data.Bits                  (xor)
import qualified Text.Megaparsec.Char.Lexer as L

import           LoadAndParse

data Entry = Entry Int Int Char String
  deriving Show

parser :: Parser Entry
parser = do
  a <- integer
  _ <- dash
  b <- integer
  _ <- space
  c <- letterChar
  _ <- colon
  _ <- space
  password <- many letterChar
  pure $ Entry a b c password

isPasswordValidPolicy1 :: Entry -> Bool
isPasswordValidPolicy1 (Entry minOccurrence maxOccurrence letter password) =
  let occurrences = length . filter (== letter) $ password
  in occurrences <= maxOccurrence && occurrences >= minOccurrence

isPasswordValidPolicy2 :: Entry -> Bool
isPasswordValidPolicy2 (Entry p1 p2 letter password) =
  (password !! (p1-1) == letter) `xor` (password !! (p2-1) == letter)

main :: IO ()
main = do
  entries <- loadAndParseAsRows parser "inputs/day02.txt"
  let validEntries = filter isPasswordValidPolicy1 entries
  putStrLn $ "Solution to part 1: " ++ tshow (length validEntries)
  let validEntries2 = filter isPasswordValidPolicy2 entries
  putStrLn $ "Solution to part 2: " ++ tshow (length validEntries2)


integer :: Parser Int
integer = L.decimal <?> "integer"

colon, dash :: Parser Char
colon = char ':'
dash = char '-'
