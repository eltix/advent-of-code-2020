
module LoadAndParse
  ( -- * Parsing
    loadAndParseAsRows
  , Parser
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  -- * Text utils
  , loadAndConvertFromTextGroups
  , splitIntoGroups
  )
  where

import           BasicPrelude
import qualified Data.Text            as T
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

loadAndParseAsRows :: Parser a -> String -> IO [a]
loadAndParseAsRows contentParser filePath = do
  unParsedEntries <- T.lines <$> readFile filePath
  mapM (parseRow contentParser) unParsedEntries

parseRow :: Parser a -> Text -> IO a
parseRow contentParser t = case runParser (contentParser <* eof) "" t of
  Right content       -> pure content
  Left someParseError -> fail $ "failed to parse content: " ++ errorBundlePretty someParseError

type Parser = Parsec Void Text

loadAndConvertFromTextGroups :: ([Text] -> a) -> String -> IO [a]
loadAndConvertFromTextGroups fromTexts filePath =
  fmap fromTexts . splitIntoGroups <$> readFile filePath

-- | Preprocess a text input file where groups are seperated by blank lines
-- and items in a group are seperated by either a newline or a space
splitIntoGroups :: Text -> [[Text]]
splitIntoGroups = fmap splitOnSpace . fmap normalize . splitOnBlankLine
  where
    splitOnBlankLine = T.splitOn "\n\n"   -- passports are separated by blank lines
    normalize        = T.replace "\n" " " -- treat newlines as spaces
    splitOnSpace     = T.words            -- pairs (key, value) are separated by spaces
