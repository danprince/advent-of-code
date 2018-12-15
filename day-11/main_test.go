package main

import "testing"

func TestPowerLevel(t *testing.T) {
	examples := []struct {
		x      int
		y      int
		serial int
		power  int
	}{
		{x: 3, y: 5, serial: 8, power: 4},
		{x: 122, y: 79, serial: 57, power: -5},
		{x: 217, y: 196, serial: 39, power: 0},
		{x: 101, y: 153, serial: 71, power: 4},
	}

	for _, example := range examples {
		actual := PowerLevel(example.x, example.y, example.serial)
		expected := example.power

		if actual != expected {
			t.Errorf(`Power Level: Expected: %#v Actual: %#v`, expected, actual)
		}
	}
}

func TestSolvePartOne(t *testing.T) {
	examples := map[string]string{
		"18":   "33,45",
		"42":   "21,61",
		"9810": "245,14",
	}

	for input, expected := range examples {
		actual := SolvePartOne(input)

		if actual != expected {
			t.Errorf(`Part 1: Expected: %#v Actual: %#v`, expected, actual)
		}
	}
}

func TestSolvePartTwo(t *testing.T) {
	examples := map[string]string{
		"18":   "90,269,16",
		"42":   "232,251,12",
		"9810": "235,206,13",
	}

	for input, expected := range examples {
		actual := SolvePartTwo(input)

		if actual != expected {
			t.Errorf(`Part 2: Expected: %#v Actual: %#v`, expected, actual)
		}
	}
}
