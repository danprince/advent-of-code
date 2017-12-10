# http://adventofcode.com/2017/day/3

function* make-spiral
  x = 0
  y = 0
  steps = 1

  loop
    [yield [x++, y] for right from 0 til steps]
    [yield [x, y--] for up from 0 til steps++]
    [yield [x--, y] for left from 0 til steps]
    [yield [x, y++] for down from 0 til steps++]

function take(n, iterator)
  [iterator.next!.value for _ til n]

function solve(n)
  spiral = make-spiral!
  squares = take n, spiral
  [x, y] = squares[* - 1]
  Math.abs(x) + Math.abs(y)

module.exports = { make-spiral }
