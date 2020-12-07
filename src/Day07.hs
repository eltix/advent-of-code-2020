module Day07 where

import           BasicPrelude
import qualified Data.HashMap.Strict        as HM
import qualified Data.Set                   as Set
import           GHC.Generics               (Generic)
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import           LoadAndParse

computeSolutions :: IO (Int, Int)
computeSolutions = do
  rules <- loadAndParseAsRows parseRule "inputs/day07.txt"
  let
    -- part 1
    bagGraph = HM.fromList [(c, cs) | (Rule c cs) <- rules]
    bagSet   = collect bagGraph (BagColor "shiny gold")
    sol1     = Set.size bagSet
    -- part 2
    sol2     = recursiveContent bagGraph (BagColor "shiny gold")
  return (sol1, sol2)

data Rule = Rule BagColor [(Int, BagColor)]
  deriving Show

newtype BagColor = BagColor String deriving (Generic, Show, Eq, Hashable, Ord)

collect :: HashMap BagColor [(Int, BagColor)] -> BagColor -> Set BagColor
collect bagGraph target = go [target]
  where
    go :: [BagColor] -> Set BagColor
    go []     = mempty
    go colors =
      let
        containsTargets :: [(Int, BagColor)] -> Bool
        containsTargets = not . null . intersect colors . fmap snd
        colors' = HM.keys . HM.filter containsTargets $ bagGraph
      in Set.union (Set.fromList colors') $ go colors'

recursiveContent :: HashMap BagColor [(Int, BagColor)] -> BagColor -> Int
recursiveContent bagGraph target = go target
  where
    go :: BagColor -> Int
    go color = sum [i * (1 + go color') | (i, color') <- bagGraph HM.! color]

--
-- Parsing
--

parseRule :: Parser Rule
parseRule = do
  container  <- manyTill L.charLiteral bagParser
  _          <- symbol "contain"
  containees <- manyTill parseContainee parseEnd
  pure $ Rule (BagColor container) containees

-- | XXX: Couldn't figure out how to remove the trailing space from the
-- bag color so I added it to the content to be matched by this parser
bagParser :: Parser ()
bagParser = lexeme $ choice
  [ () <$ symbol " bags" -- the order is paramount here: parsing 'bag'
  , () <$ symbol " bag"  -- before 'bags' would lead to an unexpected 's'
  ]

parseContainee :: Parser (Int, BagColor)
parseContainee = do
  i     <- lexeme L.decimal <?> "integer"
  color <- manyTill L.charLiteral bagParser
  _     <- lexeme $ choice [() <$ ",", () <$ "."]
  pure (i, BagColor color)

parseEnd :: Parser ()
parseEnd = choice [eof, void $ symbol "no other bags."]
