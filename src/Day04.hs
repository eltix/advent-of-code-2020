module Day04 where

import           BasicPrelude
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           LoadAndParse               (Parser)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = do
  content <- readFile "inputs/day04.txt"
  let
    passports      = fromTexts <$> preprocess content
    haveRequiredFieldsPassports = filter hasRequiredFields passports
    sol1           = length haveRequiredFieldsPassports
  putStrLn $ "Solution to part 1: " ++ tshow sol1
  let
    validPassports = filter hasValidData haveRequiredFieldsPassports
    sol2 = length validPassports
  putStrLn $ "Solution to part 2: " ++ tshow sol2

data Field = Birth | Issue | Expiration | Height | Hair | Eye | Id | Country
  deriving (Show, Eq, Generic, Hashable)

type Passport = HashMap Field Text

hasRequiredFields :: Passport -> Bool
hasRequiredFields passport =
  HM.size passport == 8                                            -- all fields are there
  || (HM.size passport == 7 && not (Country `HM.member` passport)) -- OR only the country id is missing

hasValidData :: Passport -> Bool
hasValidData p = all id
  [ intEntryInBounds (1920, 2002) $ p HM.! Birth
  , intEntryInBounds (2010, 2020) $ p HM.! Issue
  , intEntryInBounds (2020, 2030) $ p HM.! Expiration
  , validHeight (p HM.! Height)
  , validateByParsing parseHair (p HM.! Hair)
  , p HM.! Eye `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
  , validateByParsing parseId (p HM.! Id)
  ]
  where
    intEntryInBounds :: (Int, Int) -> Text -> Bool
    intEntryInBounds (a, b) t = let i = read t in i >= a && i <= b

    validHeight t = case runParser (parseHeight <* eof) "" t of
      Right (h, Cm) -> h >= 150 && h <= 193
      Right (h, In) -> h >= 59 && h <= 76
      _             -> False

-- Parsing, parsing, parsing
-- Day 4 puzzle is mostly all about parsing

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

textToEntry :: Text -> (Field, Text)
textToEntry t = case runParser parseEntry "" t of
  Right content -> content
  Left e        -> error $ "failed to parse content: " ++ errorBundlePretty e

parseEntry :: Parser (Field, Text)
parseEntry = do
  field <- parseField
  _     <- char ':'
  value <- many printChar
  pure $ (field, T.pack value)

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

-- | Check that a 'Passport' field is valid by trying to parse it
validateByParsing :: Parser a -> Text -> Bool
validateByParsing parser x = case runParser (parser <* eof) "" x of
  Right _ -> True
  Left _  -> False

parseHair :: Parser String
parseHair = do
  _ <- char '#'
  count 6 alphaNumChar

parseId :: Parser String
parseId = count 9 digitChar

data Unit = Cm | In deriving Show

parseHeight :: Parser (Int, Unit)
parseHeight = do
  magnitude <- L.decimal <?> "integer"
  unit      <- choice [Cm <$ string "cm", In <$ string "in"]
  pure (magnitude, unit)
