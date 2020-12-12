module Day11 where

import           BasicPrelude
import qualified Data.HashMap.Strict as HM

import           LoadAndParse


data Position = Floor | Empty | Occupied
  deriving Eq

type Ferry = [[Position]]
type FerryMatrix = HashMap (Int, Int) Position

toMatrix :: Ferry -> FerryMatrix
toMatrix = HM.fromList . concat . zipWith toColumn [0..] . fmap toRow
  where
    toRow :: [Position] -> [(Int, Position)]
    toRow = zip [0..]
    toColumn :: Int -> [(Int, Position)] -> [((Int, Int), Position)]
    toColumn rowIndex xs = [((rowIndex, colIndex), pos) | (colIndex, pos) <- xs]

parsePosition :: Parser Position
parsePosition = choice
  [ Floor    <$ char '.'
  , Empty    <$ char 'L'
  , Occupied <$ char '#'
  ]

fixedPoint :: (FerryMatrix -> FerryMatrix) -> FerryMatrix -> FerryMatrix
fixedPoint applyRule = go
  where
    go matrix =
      let matrix' = applyRule matrix
      in if matrix == matrix' then matrix else go matrix'

applyRule1 :: FerryMatrix -> FerryMatrix
applyRule1 previousState = HM.mapWithKey applyRule previousState
  where
    applyRule (i, j) pos
      | pos == Empty && (any id occupied)     = Empty
      | pos == Empty                          = Occupied
      | pos == Occupied && atLeast 4 occupied = Empty
      | otherwise                             = pos
      where
        occupied = ((== (Just Occupied)) . (`HM.lookup` previousState)) <$>
          [ (i-1, j-1), (i-1, j), (i-1, j+1)
          , (i  , j-1),           (i  , j+1)
          , (i+1, j-1), (i+1, j), (i+1, j+1)
          ]

-- | We could probably factorize some of this with 'previousState'
applyRule2 :: FerryMatrix -> FerryMatrix
applyRule2 mat = HM.mapWithKey applyRule mat
  where
    applyRule (i, j) pos
      | pos == Empty && (any id occupied)     = Empty
      | pos == Empty                          = Occupied
      | pos == Occupied && atLeast 5 occupied = Empty
      | otherwise                             = pos
      where occupied = adjacentOccupied (i, j) :: [Bool]

    adjacentOccupied :: (Int, Int) -> [Bool]
    adjacentOccupied (i, j) = searchDirection (i, j) <$>
      [ (-1, -1), (-1, 0), (-1, 1)
      , ( 0, -1),          ( 0 ,1)
      , ( 1, -1), ( 1, 0), ( 1, 1)
      ]
    searchDirection (i, j) (a, b) = case (i+a, j+b) `HM.lookup` mat of
      Just Floor    -> searchDirection (i+a, j+b) (a, b)
      Just Occupied -> True
      _             -> False

-- | Checks lazily that there are at least n @True@ items in a list of @Bool@
atLeast :: Int -> [Bool] -> Bool
atLeast n = (== n) . length . take n . filter id

computeSolutions :: IO (Int, Int)
computeSolutions = do
  ferry :: Ferry <- loadAndParseAsRows (many parsePosition) "inputs/day11.txt"
  let
    matrix = toMatrix ferry
    -- part 1
    fixed1 = fixedPoint applyRule1 matrix
    sol1   = HM.size . HM.filter (==Occupied) $ fixed1
    -- part 2
    fixed2 = fixedPoint applyRule2 matrix
    sol2   = HM.size . HM.filter (==Occupied) $ fixed2
  return (sol1, sol2)
