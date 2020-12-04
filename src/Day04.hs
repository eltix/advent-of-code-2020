module Day04 where

import           BasicPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import LoadAndParse (Parser)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.HashMap.Strict as HM


main :: IO ()
main = do
  content <- readFile "inputs/day04.txt"
  let
    passports      = fromTexts <$> preprocess content
    validPassports = filter isValid passports
    sol1           = length validPassports
  putStrLn $ "Solution to part 1: " ++ tshow sol1
  let
    sol2 = 0 :: Int
  putStrLn $ "Solution to part 2: " ++ tshow sol2

data Field = Birth | Issue | Expiration | Height | Hair | Eye | Id | Country
  deriving (Show, Eq, Generic, Hashable)

type Passport = HashMap Field String

isValid :: Passport -> Bool
isValid passport =
  HM.size passport == 8                                            -- all fields are there
  || (HM.size passport == 7 && not (Country `HM.member` passport)) -- OR only the country id is missing

-- parsing

-- | Preprocess the input file such that it can directly be parsed to a list of
-- 'Passport's
preprocess :: Text -> [[Text]]
preprocess = fmap splitOnSpace . fmap normalize . splitOnBlankLine
  where
    splitOnBlankLine = T.splitOn "\n\n"   -- passports are separated by blank lines
    normalize        = T.replace "\n" " " -- treat newlines as spaces
    splitOnSpace     = T.words            -- pairs (key, value) are separated by spaces

fromTexts :: [Text] -> Passport
fromTexts = HM.fromList . fmap textToEntry

textToEntry :: Text -> (Field, String)
textToEntry t = case runParser parseEntry "" t of
  Right content -> content
  Left e        -> error $ "failed to parse content: " ++ errorBundlePretty e

parseEntry :: Parser (Field, String)
parseEntry = do
  field <- parseField
  _     <- char ':'
  value <- many printChar
  pure $ (field, value)

parseField :: Parser Field
parseField = choice
  [ Birth      <$ string "byr"
  , Issue      <$ string "iyr"
  , Expiration <$ string "eyr"
  , Height     <$ string "hgt"
  , Hair       <$ string "hcl"
  , Eye        <$ string "ecl"
  , Id         <$ string "pid"
  , Country    <$ string "cid"
  ]
