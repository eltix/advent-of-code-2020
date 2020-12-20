module Day18 where

import           BasicPrelude

import LoadAndParse

computeSolutions :: IO (Int, Int)
computeSolutions = do
  -- equations <- loadAndParseAsRows parseEquation "inputs/day18.txt"
  let
    -- part 1
    sol1 = 0 -- sum . fmap evaluate $ equations
    -- part 2
    sol2 = 0
  return (sol1, sol2)

data Equation = Leaf Int | Op Op Equation Equation
  deriving Show
data Op = Plus | Times deriving Show

evaluate :: Equation -> Int
evaluate (Leaf x) = x
evaluate (Op Plus x y) = evaluate x + evaluate y
evaluate (Op Times x y) = evaluate x * evaluate y

eq :: Text
eq = "(5 * 6 * 5 * 7) + (6 + (8 * 3 * 9 + 2 + 7) + 7 + (4 * 2 + 5)) + 8"

parseEquation :: Parser Equation
parseEquation = do
  a  <- Leaf <$> int
  op <- parseOp
  b  <- try parseEquation <|> Leaf <$> int
  pure $ Op op b a

parens :: Parser a -> Parser a
parens = between (symbol ")") (symbol "(")

parseOp :: Parser Op
parseOp = lexeme $ choice [Plus <$ char '+', Times <$ char '*']
