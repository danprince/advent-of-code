package main

import "testing"

const example = `initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #`

func TestSolvePartOne(t *testing.T) {
	actual := solvePartOne(example)
	expected := 325

	if actual != expected {
		t.Errorf(`Part 1: Expected: %#v Actual: %#v`, expected, actual)
	}
}
