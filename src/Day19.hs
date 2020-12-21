{-# LANGUAGE TupleSections #-}

module Day19 where

import           BasicPrelude
import qualified Data.IntMap.Strict as I
import qualified Data.Text          as T

import           LoadAndParse


computeSolutions :: IO (Int, Int)
computeSolutions = do
  (ruleMap, messages) <- loadAndParseInput
  let
    -- part 1
    rule0         = fixpoint ruleMap
    validMessages = filter (isValid rule0) messages
    sol1          = length validMessages
    -- part 2
    sol2 = 0
  return (sol1, sol2)

-- | Checks that a message is valid by recursing down the rule and consuming the
-- message on the go
isValid :: InlinedRule -> Message -> Bool
isValid irule message = (== (Just [])) $ go message irule
  where
    go :: String -> InlinedRule -> Maybe String
    go (a:xs) (One c) = if a == c then Just xs else Nothing
    go _      (One _) = Nothing
    go xs (Several rules) = foldM go xs rules
    go xs (Or rule1 rule2) =
      case (go xs rule1, go xs rule2) of
        (Just ys, _)  -> Just ys
        (_ , Just ys) -> Just ys
        _             -> Nothing

-- | Fixed point iterations of 'inline' until Rule #0 is inlined
fixpoint :: IntMap Rule -> InlinedRule
fixpoint ruleMap = go mempty
  where
    go m = case 0 `I.lookup` m of
      Just irule -> irule
      Nothing    -> go $ oneIteration m
    oneIteration :: IntMap InlinedRule -> IntMap InlinedRule
    oneIteration m = I.union m $ I.mapMaybe (inline m) ruleMap

-- | De-reference or "inline" the rules when possible
inline :: IntMap InlinedRule -> Rule -> Maybe InlinedRule
inline _ (OneR c) = Just $ One c
inline ruleMap (SeveralR rules) = do
  irules <- traverse (`I.lookup` ruleMap) rules
  pure $ Several irules
inline ruleMap (OrR r1 r2) = do
  irules1 <- traverse (`I.lookup` ruleMap) r1
  irules2 <- traverse (`I.lookup` ruleMap) r2
  pure $ Or (Several irules1) (Several irules2)


-- | A representation of the rules as they appear in the input
data Rule = OneR Char | SeveralR References | OrR References References
  deriving Show

-- | De-referenced or "inlined" rule where the reference to a rule through its ID
-- has been replaced by the actual rule.
-- It may be possible to merge this with 'Rule' into a single polymorphic
-- data-type
data InlinedRule =
  One Char
  | Several [InlinedRule]
  | Or InlinedRule InlinedRule
  deriving Show

type References = [Int]
type Message = String

parseRule :: Parser (Int, Rule)
parseRule = do
  i    <- int
  _    <- symbol ":"
  rule <- parseSingle <|> (try parseOr) <|> (SeveralR <$> parseSub)
  pure (i, rule)
  where
    parseSingle  = OneR <$> between (symbol "\"") (symbol "\"") letterChar
    parseSub     = many int
    parseOr = OrR <$> parseSub <* symbol "|" <*> parseSub

loadAndParseInput :: IO (IntMap Rule, [Message])
loadAndParseInput = do
  rules':messages':[] <- (fmap T.lines . T.splitOn "\n\n") <$> readFile "./inputs/day19.txt"
  rules <- mapM (parseRow parseRule) rules'
  pure (I.fromList rules, T.unpack <$> messages')
