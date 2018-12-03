package main

import (
	"strings"
	"testing"
)

const example = `+1
-2
+3
+1`

func TestSolvePartOne(t *testing.T) {
	actual := SolvePartOne(example)
	expected := 3

	if actual != expected {
		t.Errorf(`Expected: %d Actual: %d`, expected, actual)
	}
}

func TestSolvePartTwo(t *testing.T) {
	examples := []struct {
		input  string
		output int
	}{
		{input: "+1, -1", output: 0},
		{input: "+3, +3, +4, -2, -4", output: 10},
		{input: "-6, +3, +8, +5, -6", output: 5},
		{input: "+7, +7, -2, -7, -4", output: 14},
	}

	for _, example := range examples {
		input := strings.Replace(example.input, ", ", "\n", -1)
		actual := SolvePartTwo(input)
		expected := example.output

		if actual != expected {
			t.Errorf(`Expected: %d Actual: %d`, expected, actual)
		}
	}
}
