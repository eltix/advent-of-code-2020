
module Utils.LoadAndParse
  ( loadAndParseAsRows
  , Parser
  , module            Text.Megaparsec
  , module Text.Megaparsec.Char
  )
  where

import           BasicPrelude
import qualified Data.Text                  as T
import           Data.Void                  (Void)
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
