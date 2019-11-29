package main

import (
	"strings"
	"testing"
)

var example = strings.TrimSpace(`
/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   
`)

func TestSolvePartOne(t *testing.T) {
	actual := solvePartOne(example)
	expected := "7,3"

	if actual != expected {
		t.Errorf(`Part 1: Expected: %#v Actual: %#v`, expected, actual)
	}
}

func TestSolvePartTwo(t *testing.T) {
	examples := map[string]string{}

	for input, expected := range examples {
		actual := solvePartTwo(input)

		if actual != expected {
			t.Errorf(`Part 2: Expected: %#v Actual: %#v`, expected, actual)
		}
	}
}
