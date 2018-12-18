package main

import "testing"

func TestSolvePartOne(t *testing.T) {
	examples := map[string]int{
		"aA":               len(""),
		"abBA":             len(""),
		"abAB":             len("abAB"),
		"aabAAB":           len("aabAAB"),
		"dabAcCaCBAcCcaDA": len("dabCBAcaDA"),
	}

	for input, expected := range examples {
		actual := solvePartOne(input)

		if actual != expected {
			t.Errorf(`Part 1: Expected: %#v Actual: %#v`, expected, actual)
		}
	}
}

func TestSolvePartTwo(t *testing.T) {
	example := `dabAcCaCBAcCcaDA`
	expected := 4
	actual := solvePartTwo(example)

	if actual != expected {
		t.Errorf(`Part 2: Expected: %#v Actual: %#v`, expected, actual)
	}
}
