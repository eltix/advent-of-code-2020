module Day06 where

import           BasicPrelude
import qualified Data.Set     as Set
import qualified Data.Text    as T

newtype Group = Group [Person] deriving Show
newtype Person = Person (Set Char) deriving Show

computeSolutions :: IO (Int, Int)
computeSolutions = do
  groups :: [Group] <- fmap toGroup . preprocess <$> readFile "inputs/day06.txt"
  let
    -- part 1
    groupsAnswers = anyoneSaidYesInGroup <$> groups
    sol1 = sum . fmap Set.size $ groupsAnswers
    -- part 2
    groupsAnswers2 = everyoneSaidYesInGroup <$> groups
    sol2 = sum . fmap Set.size $ groupsAnswers2
  return (sol1, sol2)

anyoneSaidYesInGroup :: Group -> Set Char
anyoneSaidYesInGroup (Group persons) = Set.unions [answers | (Person answers) <- persons]

everyoneSaidYesInGroup :: Group -> Set Char
everyoneSaidYesInGroup (Group persons) = foldl1' Set.intersection [answers | (Person answers) <- persons]

toGroup :: [Text] -> Group
toGroup = Group . fmap (Person . Set.fromList . T.unpack)

-- | Preprocess the input file such that it can directly be parsed to a list of
-- 'Passport's
preprocess :: Text -> [[Text]]
preprocess = fmap splitOnSpace . fmap normalize . splitOnBlankLine
  where
    splitOnBlankLine = T.splitOn "\n\n"   -- passports are separated by blank lines
    normalize        = T.replace "\n" " " -- treat newlines as spaces
    splitOnSpace     = T.words            -- pairs (key, value) are separated by spaces
