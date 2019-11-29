package main

import "testing"

func TestSolvePartOne(t *testing.T) {
	examples := map[string]string{
		"9":    "5158916779",
		"5":    "0124515891",
		"18":   "9251071085",
		"2018": "5941429882",
	}

	for input, expected := range examples {
		actual := solvePartOne(input)

		if actual != expected {
			t.Errorf(`Part 1: Expected: %#v Actual: %#v`, expected, actual)
		}
	}
}

func TestSolvePartTwo(t *testing.T) {
	examples := map[string]string{
		"51589": "9",
		"01245": "5",
		"92510": "18",
		"59414": "2018",
	}

	for input, expected := range examples {
		actual := solvePartTwo(input)

		if actual != expected {
			t.Errorf(`Part 2: Expected: %#v Actual: %#v`, expected, actual)
		}
	}
}
