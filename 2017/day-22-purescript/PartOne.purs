module Main where

import Prelude
import Virus

update :: Cell -> Cell
update Clean = Infected
update _ = Clean

turn :: Cell -> Direction -> Direction
turn Clean = turnAntiClockwise
turn _ = turnClockwise
turn Flagged = turnReverse
turn Weakened = id

solve :: String -> Int
solve input =
  end.infections
  where
    world = createWorld update turn input
    end = bursts 10000 world

