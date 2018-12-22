package main

import (
	"strings"
	"testing"
)

func TestSolvePartOne(t *testing.T) {
	examples := map[string]int{
		`#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########`: 27828,
		//		`
		//#######
		//#.G...#
		//#...EG#
		//#.#.#G#
		//#..G#E#
		//#.....#
		//#######
		//`: 27730,
	}

	for input, expected := range examples {
		actual := solvePartOne(strings.TrimSpace(input))

		if actual != expected {
			t.Errorf(`Part 1: Expected: %#v Actual: %#v`, expected, actual)
		}
	}
}

func TestSolvePartTwo(t *testing.T) {
	examples := map[string]int{}

	for input, expected := range examples {
		actual := solvePartTwo(input)

		if actual != expected {
			t.Errorf(`Part 2: Expected: %#v Actual: %#v`, expected, actual)
		}
	}
}
