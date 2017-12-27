-- http://adventofcode.com/2017/day/21

module PartOne (solve) where

import Prelude

import Data.Maybe
import Data.Array
import Data.String as String
import Data.Map as Map
import Data.Int (even)

type Rule = { from :: Pattern, to :: Pattern }
type Pattern = Array (Array Char)
type Rules = Map.Map Pattern Pattern
type Grid = Array Pattern

-- Parsing Utils --

rulesFromString :: String -> Rules
rulesFromString src =
  let
    lines = String.split (String.Pattern "\n") src
    rules = map ruleFromString lines
  in
    foldl addRule Map.empty rules

addRule :: Rules -> Rule -> Rules
addRule map rule =
  let
    alternatives = equivalentPatterns rule.from
    insert = \rules from -> Map.insert from rule.to rules
  in
    foldl insert map alternatives

ruleFromString :: String -> Rule
ruleFromString src =
  case String.split (String.Pattern " => ") src of
    [from, to] -> { from: patternFromString from, to: patternFromString to }
    _          -> { from: [], to: [] }

patternFromString :: String -> Pattern
patternFromString src =
  let rows = String.split (String.Pattern "/") src in
  map String.toCharArray rows

-- Expansion Utils --

equivalentPatterns :: Pattern -> Array Pattern
equivalentPatterns grid =
  concatMap rotations $ flips grid

flips :: Pattern -> Array Pattern
flips [[a, b, c], [d, e, f], [g, h, i]] = [
      [[a, b, c], [d, e, f], [g, h, i]],
      [[g, h, i], [d, e, f], [a, b, c]],
      [[c, b, a], [f, e, d], [i, h, g]]]
flips [[a, b], [c, d]] = [
      [[a, b], [c, d]],
      [[b, a], [d, c]],
      [[c, d], [a, b]]]
flips _ = []

rotations :: Pattern -> Array Pattern
rotations [[a, b, c], [d, e, f], [g, h, i]] =
  [[[a, b, c], [d, e, f], [g, h, i]],
   [[g, d, a], [h, e, b], [i, f, c]],
   [[i, h, g], [f, e, d], [c, b, a]],
   [[c, f, i], [b, e, h], [a, d, g]]]
rotations [[a, b], [c, d]] =
  [[[a, b], [c, d]],
   [[d, a], [c, b]],
   [[c, d], [b, a]],
   [[b, c], [a, d]]]
rotations _ = []

-- Solution

groupsOf :: forall a. Int -> Array a -> Array (Array a)
groupsOf n [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

-- FIXME: Generates the correct grids, but in the wrong order
patternToGrid :: Int -> Pattern -> Grid
patternToGrid n pattern =
  if all null pattern then [] else let
    xs = map (take n) pattern
    ys = map (drop n) pattern
  in
    concat [groupsOf n xs, patternToGrid n ys]

-- FIXME: Doesn't work at all (because of patternToGrid ordering)
gridToPattern :: Int -> Grid -> Pattern
gridToPattern n grid =
  if all null grid then [] else let
    xs = concat $ map concat grid
  in
    groupsOf n xs

findReplacement :: Rules -> Pattern -> Maybe Pattern
findReplacement rules pattern =
  Map.lookup pattern rules

expandPattern :: Rules -> Pattern -> Int -> Pattern
expandPattern rules pattern 0 = pattern
expandPattern rules pattern times =
  let
    n = length pattern
    size = if even n then 2 else 3
    grid = patternToGrid size pattern
    replacements = mapMaybe (findReplacement rules) grid
  in
    gridToPattern (size + 1) replacements

solve :: String -> Int -> Int
solve input iterations =
  let
    rules = rulesFromString input
    pattern = patternFromString ".#./..#/###"
  in
    expandPattern rules pattern iterations

