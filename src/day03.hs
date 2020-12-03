module Main where

import           BasicPrelude

import           LoadAndParse

data Tile = Open | Tree
  deriving (Show, Eq)

parseTile :: Parser Tile
parseTile = choice
  [ Open <$ char '.'
  , Tree <$ char '#'
  ]

-- | Collect tiles encountered when going down the toboggan with slope
-- (Right @slope@, Down 1)
toboggan :: Int -> [[Tile]] -> [Tile]
toboggan slope = fmap (uncurry oneMoveDown) . zip [0,slope..]

oneMoveDown :: Int -> [Tile] -> Tile
oneMoveDown horizontalOffset row = infiniteRow !! horizontalOffset
  -- this list is infinite but it's ok because we don't evaluate it fully and Haskell is lazy :)
  where infiniteRow = concat . repeat $ row

-- | How many trees are encountered on toboggan with given slope going down
-- a given landscape
treesEncountered ::
  [[Tile]] -- ^ landscape
  -> Int   -- ^ toboggan slope
  -> Int   -- ^ number of trees encountered
treesEncountered landscape = length . filter (== Tree) . (flip toboggan) landscape

main :: IO ()
main = do
  landscape :: [[Tile]] <- loadAndParseAsRows (many parseTile) "inputs/day03.txt"
  -- part 1
  let numTreesPart1 = treesEncountered landscape 3
  putStrLn $ "Solution to part 1: " ++ tshow numTreesPart1
  -- part 2
  let
    -- let's go downhill with different toboggan slopes. Weeeeeeeeeee
    slopes   = [1,3,5,7]
    numTrees = treesEncountered landscape <$> slopes
    -- the last toboggan (Right 1, Down 2) is the same as a (Right 1, Down 1)
    -- toboggan on a half-landscape with every other row skipped
    halfLandscape         = [landscape!!i | i <- [(0::Int),2 .. (length(landscape)-1)]]
    numTreesSteepToboggan = treesEncountered halfLandscape 1
    allNumTrees           = product $ numTreesSteepToboggan: numTrees
  putStrLn $ "Solution to part 2: " ++ tshow allNumTrees
