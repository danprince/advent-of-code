-- http://adventofcode.com/2017/day/13

module PartOne exposing (calculateSeverity)

import List

getSlidingIndex : Int -> Int -> Int
getSlidingIndex index length =
  let
    maxIndex = length - 1
    step = index % (maxIndex * 2)
    offset = abs (step - maxIndex)
  in
    maxIndex - offset

calculateSeverity : (Int, Int) -> Int
calculateSeverity layer =
  let
    (step, length) = layer
    scanner = getSlidingIndex step length
  in
    if scanner == 0 then step * length else 0

solve : List (Int, Int) -> Int
solve layers =
  layers
    |> List.map getSeverity
    |> List.sum

