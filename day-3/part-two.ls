# http://adventofcode.com/2017/day/3

{ make-spiral } = require("./part-one")

function take-one(iterator)
  iterator.next!.value

function sample(x, y, grid)
  grid[[x, y]] or 0

function sum-adjacent(x, y, grid)
  (sample x - 1, y - 1, grid) +
  (sample x - 0, y - 1, grid) +
  (sample x + 1, y - 1, grid) +
  (sample x - 1, y - 0, grid) +
  (sample x + 1, y - 0, grid) +
  (sample x - 1, y + 1, grid) +
  (sample x - 0, y + 1, grid) +
  (sample x + 1, y + 1, grid)

function solve(n)
  spiral = make-spiral!
  value = 0
  grid = {}

  while value < n
    [x, y] = take-one spiral
    value = sum-adjacent x, y, grid or 1
    grid[[x, y]] = value

  value
