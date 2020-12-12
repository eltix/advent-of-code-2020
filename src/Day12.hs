{-# LANGUAGE LambdaCase #-}

module Day12 where

import           BasicPrelude hiding (Either (..))
import           Lens.Micro

import           LoadAndParse

computeSolutions :: IO (Int, Int)
computeSolutions = do
  instructions <- loadAndParseAsRows parseInstruction "inputs/day12.txt"
  let
    origin = (0, 0)
    -- part 1
    lastState = foldl' move (origin, East) instructions
    sol1      = manhattan origin $ fst lastState
    -- part 2
    lastState2 = foldl' moveWithWaypoint (origin, (10, 1)) instructions
    sol2       = manhattan origin $ fst lastState2
  return (sol1, sol2)

move :: ((Int, Int), Cardinal) -> Instruction -> ((Int, Int), Cardinal)
move (boat, orientation) = \case
  Translation card n -> (translate card n boat       , orientation                   )
  Rotation dir qts   -> (boat                        , rotateBoat dir qts orientation)
  Forward n          -> (translate orientation n boat, orientation                   )

translate :: Cardinal -> Int -> (Int, Int) -> (Int, Int)
translate North n = over _2 (+n)
translate South n = over _2 (subtract n)
translate East  n = over _1 (+n)
translate West  n = over _1 (subtract n)

rotateBoat :: Direction -> Int -> Cardinal -> Cardinal
rotateBoat Right n = toEnum . (`mod` 4) . (+n)         . fromEnum
rotateBoat Left  n = toEnum . (`mod` 4) . (subtract n) . fromEnum

-- | Type alias for the pair (boat position, waypoint)
type BoatContext = ((Int, Int), (Int, Int))

moveWithWaypoint :: BoatContext -> Instruction -> BoatContext
moveWithWaypoint (boat, waypoint) = \case
  Translation card n -> (boat                          , translate card n waypoint        )
  Rotation dir qts   -> (boat                          , rotateAroundShip dir qts waypoint)
  Forward n          -> (moveToWaypoint waypoint n boat, waypoint                         )

rotateAroundShip :: Direction -> Int -> (Int, Int) -> (Int, Int)
rotateAroundShip Right n = nQuarterTurnsClockWise (n `mod` 4)
  where
    quarterTurnClockWise (x, y) = (y, -x)
    nQuarterTurnsClockWise k x  = iterate quarterTurnClockWise x !! k
rotateAroundShip Left n = rotateAroundShip Right (-n)

moveToWaypoint :: (Int, Int) -> Int -> (Int, Int) -> (Int, Int)
moveToWaypoint (xw, yw) n = translate East (n*xw) . translate North (n*yw)

data Instruction =
  Translation Cardinal Int
  | Rotation Direction Int
  | Forward Int
  deriving Show

-- | Cardinal points listed in clock-wise order such that
-- @toEnum . (`mod` 4) . (+1) . fromEnum@ rotates clock-wise 90 degrees
-- >>> toEnum . (`mod` 4) . (+1) . fromEnum $ West
-- North
-- >>> toEnum . (`mod` 4) . (subtract 2) . fromEnum $ East
-- West
data Cardinal = North | East | South | West deriving (Show, Enum)

data Direction = Left | Right deriving Show

parseInstruction :: Parser Instruction
parseInstruction = parseTranslation <|> parseRotation <|> parseForward
  where
    parseTranslation = Translation
      <$> choice [North <$ char 'N', South <$ char 'S', East <$ char 'E', West <$ char 'W']
      <*> signedInt
    parseRotation = Rotation
      <$> choice [Left <$ char 'L', Right <$ char 'R']
      <*> choice [1 <$ string "90", 2 <$ string "180", 3 <$ string "270"]
    parseForward =
      char 'F'
      *> fmap Forward signedInt

-- | Manhattan distance in \(\mathbb N^2\)
manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (xa, ya) (xb, yb) = abs (xb-xa) + abs (yb-ya)
