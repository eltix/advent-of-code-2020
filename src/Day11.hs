module Day11 where

import           BasicPrelude
import           Control.DeepSeq     (NFData, ($!!))
import qualified Data.HashMap.Strict as HM
import           GHC.Generics

import           LoadAndParse


data Position = Floor | Empty | Occupied
  deriving (Eq, Generic, NFData)

type Ferry = [[Position]]
type FerryMatrix = HashMap (Int, Int) Position

toMatrix :: Ferry -> FerryMatrix
toMatrix = HM.fromList . concat . zipWith toColumn [0..] . fmap toRow
  where
    toRow :: [Position] -> [(Int, Position)]
    toRow = zip [0..]
    toColumn :: Int -> [(Int, Position)] -> [((Int, Int), Position)]
    toColumn rowIndex xs = [((rowIndex, colIndex), pos) | (colIndex, pos) <- xs]
--
-- fromMatrix :: FerryMatrix -> Ferry
-- fromMatrix matrix = res
--   where
--     keys = HM.keys matrix
--     numRows = maximum . fst <$> keys
--     numCols = maximum . snd  <$> keys


instance Show Position where
  show Floor    = "."
  show Empty    = "L"
  show Occupied = "#"

parsePosition :: Parser Position
parsePosition = choice
  [ Floor    <$ char '.'
  , Empty    <$ char 'L'
  , Occupied <$ char '#'
  ]

fixedPoint :: (FerryMatrix -> FerryMatrix) -> FerryMatrix -> FerryMatrix
fixedPoint applyRule = go
  where
    go matrix | matrix == applyRule matrix = matrix
              | otherwise                  = go $!! applyRule matrix

applyRule1 :: FerryMatrix -> FerryMatrix
applyRule1 previousState = HM.mapWithKey applyRule previousState
  where
    applyRule (i, j) pos
      | looksGood (i, j) pos  = Occupied
      | tooCrowded (i, j) pos = Empty
      | otherwise             = pos
    looksGood (i, j) pos =
      pos == Empty && (null . filter (== (Just Occupied)) $ adjacent (i, j))
    tooCrowded (i, j) pos =
      pos == Occupied && ((length . filter (== (Just Occupied)) $ adjacent (i, j)) >= 4)
    adjacent (i, j) = (`HM.lookup` previousState) <$>
      [ (i-1, j-1), (i-1, j), (i-1, j+1)
      , (i  , j-1),           (i  , j+1)
      , (i+1, j-1), (i+1, j), (i+1, j+1)
      ]

-- | We could probably factorize some of this with 'previousState'
applyRule2 :: FerryMatrix -> FerryMatrix
applyRule2 mat = HM.mapWithKey applyRule mat
  where
    applyRule (i, j) pos
      | looksGood (i, j) pos  = Occupied
      | tooCrowded (i, j) pos = Empty
      | otherwise             = pos
    looksGood (i, j) pos  = pos == Empty && (all not $ occupied (i, j))
    tooCrowded (i, j) pos =
      pos == Occupied && ((length . filter id $ occupied (i, j)) >= 5)
    occupied :: (Int, Int) -> [Bool]
    occupied (i, j) = searchDirection (i, j) <$>
      [ (-1, -1), (-1, 0), (-1, 1)
      , ( 0, -1),          ( 0 ,1)
      , ( 1, -1), ( 1, 0), ( 1, 1)
      ]

    searchDirection (i, j) (a, b) = case (i+a, j+b) `HM.lookup` mat of
      Just Floor    -> searchDirection (i+a, j+b) (a, b)
      Just Occupied -> True
      _             -> False

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
