package main

import "testing"

func TestSolvePartOne(t *testing.T) {
	examples := map[string]int{
		"9 players; last marble is worth 25 points":    32,
		"10 players; last marble is worth 1618 points": 8317,
		"13 players; last marble is worth 7999 points": 146373,
		"17 players; last marble is worth 1104 points": 2764,
		"21 players; last marble is worth 6111 points": 54718,
		"30 players; last marble is worth 5807 points": 37305,
	}

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
