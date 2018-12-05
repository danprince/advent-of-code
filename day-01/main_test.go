package main

import (
	"strings"
	"testing"
)

func TestSolvePartOne(t *testing.T) {
	example := strings.Replace("+1, -2, +3, +1", ", ", "\n", -1)
	expected := 3
	actual := SolvePartOne(example)

	if actual != expected {
		t.Errorf(`Expected: %d Actual: %d`, expected, actual)
	}
}

func TestSolvePartTwo(t *testing.T) {
	examples := map[string]int{
		"+1, -1":             0,
		"+3, +3, +4, -2, -4": 10,
		"-6, +3, +8, +5, -6": 5,
		"+7, +7, -2, -7, -4": 14,
	}

	for input, output := range examples {
		input = strings.Replace(input, ", ", "\n", -1)
		actual := SolvePartTwo(input)
		expected := output

		if actual != expected {
			t.Errorf(`Expected: %d Actual: %d`, expected, actual)
		}
	}
}
