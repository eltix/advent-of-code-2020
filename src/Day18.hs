module Day18 where

import           BasicPrelude
import           Control.Monad.Combinators.Expr
import           LoadAndParse

computeSolutions :: IO (Int, Int)
computeSolutions = do
  -- part 1
  equations <- loadAndParseAsRows (parseEquation rulesPart1) "inputs/day18.txt"
  let sol1 = sum . fmap evaluate $ equations
  -- part 2
  equations' <- loadAndParseAsRows (parseEquation rulesPart2) "inputs/day18.txt"
  let sol2 = sum . fmap evaluate $ equations'
  return (sol1, sol2)

data Equation = Leaf Int | Op Op Equation Equation
  deriving Show
data Op = Plus | Times deriving Show

evaluate :: Equation -> Int
evaluate (Leaf x)       = x
evaluate (Op Plus x y)  = evaluate x + evaluate y
evaluate (Op Times x y) = evaluate x * evaluate y

-- | Believe it or not I had the hardest time implementing an equation parser,
-- especially because of these forsaken parentheses.
-- I gave up and used 'Control.Monad.Combinators.Expr.makeExprParser' instead
-- which turned out to be extremely easy and satisfying.
parseEquation :: [[Operator Parser Equation]] -> Parser Equation
parseEquation rules = makeExprParser term rules
  where
    term :: Parser Equation
    term = parens (parseEquation rules) <|> (Leaf <$> int) <?> "term"

-- | + and * are left-associative and have same precedence level
rulesPart1 :: [[Operator Parser Equation]]
rulesPart1 =
  [ [ InfixL ((Op Plus) <$ symbol "+"), InfixL ((Op Times) <$ symbol "*") ] ]

-- | same as part1 but + has precedence over *
rulesPart2 :: [[Operator Parser Equation]]
rulesPart2 =
  [ [InfixL ((Op Plus) <$ symbol "+") ]
  , [InfixL ((Op Times) <$ symbol "*")]
  ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
