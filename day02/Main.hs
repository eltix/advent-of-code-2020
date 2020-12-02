{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           BasicPrelude
import           Data.Bits                  (xor)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

isPasswordValidPolicy1 :: Entry -> Bool
isPasswordValidPolicy1 (Entry minOccurrence maxOccurrence letter password) =
  let occurrences = length . filter (== letter) $ password
  in occurrences <= maxOccurrence && occurrences >= minOccurrence

isPasswordValidPolicy2 :: Entry -> Bool
isPasswordValidPolicy2 (Entry p1 p2 letter password) =
  (password !! (p1-1) == letter) `xor` (password !! (p2-1) == letter)

main :: IO ()
main = do
  entries <- loadEntries
  let validEntries = filter isPasswordValidPolicy1 entries
  putStrLn $ "Solution to part 1: " ++ tshow (length validEntries)
  let validEntries2 = filter isPasswordValidPolicy2 entries
  putStrLn $ "Solution to part 2: " ++ tshow (length validEntries2)

loadEntries :: IO [Entry]
loadEntries = do
  unParsedEntries <- T.lines <$> readFile "inputs/day02.txt"
  mapM parseEntry unParsedEntries

parseEntry :: Text -> IO Entry
parseEntry t = case runParser (parser <* eof) "" t of
  Right entry         -> pure entry
  Left someParseError -> fail $ "failed to parse Distrib input: " ++ errorBundlePretty someParseError

type Parser = Parsec Void Text

integer :: Parser Int
integer = L.decimal <?> "integer"

colon, dash :: Parser Char
colon = char ':'
dash = char '-'

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
