module Main where

import Prelude
import Virus

update :: Cell -> Cell
update Clean = Weakened
update Weakened = Infected
update Infected = Flagged
update Flagged = Clean

turn :: Cell -> Direction -> Direction
turn Clean = turnAntiClockwise
turn Infected = turnClockwise
turn Flagged = turnReverse
turn Weakened = id

solve :: String -> Int
solve input =
  end.infections
  where
    world = createWorld update turn input
    end = bursts 10000000 world

