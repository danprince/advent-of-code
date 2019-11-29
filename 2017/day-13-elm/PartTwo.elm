-- http://adventofcode.com/2017/day/13

module PartTwo

import List
import PartOne exposing (calculateSeverity)

incrementStep : Int -> (Int, Int) -> (Int, Int)
incrementStep amount layer =
  let
    (step, length) = layer
  in
    (step + amount, length)

canPassFirewall : List (Int, Int) -> Int -> Bool
canPassFirewall layers delay =
  layers
    |> List.map (incrementStep delay)
    |> List.map calculateSeverity
    |> List.sum
    |> (==) 0

solveWithDelay : List (Int, Int) -> Int -> Int
solveWithDelay layers delay =
  if canPassFirewall layers delay then
    delay
  else
    solveWithDelay layers (delay + 1)

solve : List (Int, Int) -> Int
solve layers = solveWithDelay layers 0
