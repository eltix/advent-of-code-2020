
module LoadAndParse
  ( -- * Parsing
    loadAndParseAsRows
  , Parser
  , unsafeParse
  , lexeme, symbol, signedInt, int
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , parseRow
  -- * Text utils
  , loadAndConvertFromTextGroups
  , splitIntoGroups
  )
  where

import           BasicPrelude               hiding (empty)
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

loadAndParseAsRows :: Parser a -> String -> IO [a]
loadAndParseAsRows contentParser filePath = do
  unParsedEntries <- T.lines <$> readFile filePath
  mapM (parseRow contentParser) unParsedEntries

parseRow :: Parser a -> Text -> IO a
parseRow contentParser t = case runParser (contentParser <* eof) "" t of
  Right content       -> pure content
  Left someParseError -> fail $ "failed to parse content: " ++ errorBundlePretty someParseError

type Parser = Parsec Void Text

-- | Turn a parser into a lexeme, i.e. make it consume and ignore the
-- whitespace after the main content.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | space consumer
sc :: Parser ()
sc = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol sc

int :: Parser Int
int = lexeme L.decimal

signedInt :: Parser Int
signedInt = L.signed sc L.decimal

loadAndConvertFromTextGroups :: ([Text] -> a) -> String -> IO [a]
loadAndConvertFromTextGroups fromTexts filePath =
  fmap fromTexts . splitIntoGroups <$> readFile filePath

-- | XXX This function is partial. Use at your own risk.
-- Typically used when we're too lazy to propagate 'Either'
unsafeParse :: Parser a -> Text -> a
unsafeParse parser = fromJust . parseMaybe parser

-- | Preprocess a text input file where groups are seperated by blank lines
-- and items in a group are seperated by either a newline or a space
splitIntoGroups :: Text -> [[Text]]
splitIntoGroups = fmap splitOnSpace . fmap normalize . splitOnBlankLine
  where
    splitOnBlankLine = T.splitOn "\n\n"   -- passports are separated by blank lines
    normalize        = T.replace "\n" " " -- treat newlines as spaces
    splitOnSpace     = T.words            -- pairs (key, value) are separated by spaces
