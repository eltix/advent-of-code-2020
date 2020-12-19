module Day16 where

import           BasicPrelude

import qualified Data.Text    as T
import           LoadAndParse

computeSolutions :: IO (Int, Maybe Int)
computeSolutions = do
  (rules, myTicket, nearbyTickets) <- loadInput
  let
    -- part 1
    sol1 = sum . fmap sum . fmap (invalidValues rules) $ nearbyTickets
    -- part 2
    validTickets = filter (null . invalidValues rules) $ nearbyTickets
    validColumns = findValidColumnsPerRule validTickets rules
    maybeMapping = matchRuleWithColumn validColumns
    sol2 = do
      mapping <- maybeMapping
      pure $ product [myTicket !! (mapping !! i) | i <- [0..5]]
  return (sol1, sol2)

invalidValues :: [Rule] -> Ticket -> [Int]
invalidValues rules = filter invalidForAllRules
  where
    invalidForAllRules n = all (not . validForRule n) rules

validForRule :: Int -> Rule -> Bool
validForRule n (Rule _ ranges) = or [n >= a && n <= b | (a, b) <- ranges]

findValidColumnsPerRule :: [Ticket] -> [Rule] -> [[Int]]
findValidColumnsPerRule tickets = fmap findColumn
  where
    columnsWithIndices :: [(Int, [Int])]
    columnsWithIndices = zip [0..] $ transpose tickets
    -- | Find the columnds where all values are valid for this rule
    findColumn :: Rule -> [Int]
    findColumn rule = fst <$> filter (all (flip validForRule rule) . snd) columnsWithIndices

-- | Try to match one rule with one column. The function returns a mapping wrapped
-- in a @Maybe@ because there is no guarantee to find a solution
matchRuleWithColumn :: [[Int]] -> Maybe [Int]
matchRuleWithColumn validColumns = fmap snd . sortOn fst <$> foldM f [] xs
  where
    xs = sortOn (length . snd) $ zip [0..] validColumns :: [(Int, [Int])]
    f :: [(Int, Int)] -> (Int, [Int]) -> Maybe [(Int, Int)]
    f acc (ruleIndex, ruleValidColumns) = do
      c <- find (`notElem` (snd <$> acc)) ruleValidColumns
      pure $ (ruleIndex, c):acc

loadInput :: IO ([Rule], Ticket, [Ticket])
loadInput = do
  rules':ticket':nearbyTickets':[] <- (fmap T.lines . T.splitOn "\n\n") <$> readFile "./inputs/day16.txt"
  rules <- mapM (parseRow parseRule) rules'
  let
    ticket = fmap read . T.splitOn "," $ ticket' !! 1
    nearbyTickets = fmap read . T.splitOn "," <$> tail nearbyTickets'
  pure (rules, ticket, nearbyTickets)

data Rule = Rule Text [(Int, Int)]
type Ticket = [Int]

parseRule :: Parser Rule
parseRule = do
  label' <- manyTill (letterChar <|> spaceChar) (string ": ")
  a <- signedInt
  _ <- char '-'
  b <- signedInt
  _ <- symbol " or "
  c <- signedInt
  _ <- char '-'
  d <- signedInt
  pure $ Rule (T.pack label') [(a, b), (c, d)]
