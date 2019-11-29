module Virus
  ( Direction(..)
  , Cell(..)
  , createWorld
  , bursts
  , turnAntiClockwise
  , turnClockwise
  , turnReverse
  ) where

import Prelude
import Data.Array (mapWithIndex)
import Data.Map (Map, insert, lookup, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, toCharArray, indexOf)
import Data.Tuple (Tuple(..))

data Direction = North | East | South | West
data Cell = Clean | Weakened | Infected | Flagged
type Point = Tuple Int Int
type Grid = Map Point Cell
type UpdateRule = Cell -> Cell
type TurnRule = Cell -> Direction -> Direction
type World =
  { grid :: Grid
  , carrier :: Point
  , direction :: Direction
  , infections :: Int
  , updateRule :: UpdateRule
  , turnRule :: TurnRule
  }

cellFromString :: Char -> Cell
cellFromString '#' = Infected
cellFromString _ = Clean

zipWithIndices :: forall a. Array a -> Array (Tuple Int a)
zipWithIndices = mapWithIndex Tuple

parseGrid :: String -> Grid
parseGrid input =
  fromFoldable $ do
    Tuple y row <- zipWithIndices $ split (Pattern "\n") input
    Tuple x cell <- zipWithIndices $ toCharArray row
    [Tuple (Tuple x y) (cellFromString cell)]

findCenter :: String -> Point
findCenter input =
  case indexOf (Pattern "\n") input of
    Just n -> Tuple (n / 2) (n / 2)
    Nothing -> Tuple 0 0

moveInDirection :: Direction -> Point -> Point
moveInDirection West  (Tuple x y) = Tuple (x - 1) (y + 0)
moveInDirection North (Tuple x y) = Tuple (x + 0) (y - 1)
moveInDirection East  (Tuple x y) = Tuple (x + 1) (y + 0)
moveInDirection South (Tuple x y) = Tuple (x + 0) (y + 1)

turnClockwise :: Direction -> Direction
turnClockwise West = North
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West

turnAntiClockwise :: Direction -> Direction
turnAntiClockwise West = South
turnAntiClockwise South = East
turnAntiClockwise East = North
turnAntiClockwise North = West

turnReverse :: Direction -> Direction
turnReverse = turnClockwise >>> turnClockwise

currentCell :: World -> Cell
currentCell world =
  case lookup world.carrier world.grid of
    Just isInfected -> isInfected
    Nothing -> Clean

updateCell :: World -> World
updateCell world =
  world { grid = grid, infections = world.infections + infections }
  where
  cell = world.updateRule (currentCell world)
  infections = case cell of
    Infected -> 1
    _ -> 0
  grid = insert world.carrier cell world.grid

moveCarrier :: World -> World
moveCarrier world =
  world { carrier = moveInDirection world.direction world.carrier }

turnCarrier :: World -> World
turnCarrier world =
  world { direction = turn world.direction }
  where
  turn = world.turnRule (currentCell world)

burst :: World -> World
burst = turnCarrier >>> updateCell >>> moveCarrier

bursts :: Int -> World -> World
bursts 0 world = world
bursts n world = bursts (n - 1) (burst world)

createWorld :: UpdateRule -> TurnRule -> String -> World
createWorld update turn input =
  { grid: parseGrid input
  , carrier: findCenter input
  , direction: North
  , infections: 0
  , updateRule: update
  , turnRule: turn
  }

