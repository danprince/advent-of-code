package main

import "testing"

func TestSolvePartOne(t *testing.T) {
	examples := map[string]int{}

	for input, expected := range examples {
		actual := solvePartOne(input)

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
