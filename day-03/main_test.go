package main

import "testing"

const example = `#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2`

func TestSolvePartOne(t *testing.T) {
	expected := 4
	actual := SolvePartOne(example)

	if actual != expected {
		t.Errorf(`Part 1: Expected: %d Actual: %d`, expected, actual)
	}
}

func TestSolvePartTwo(t *testing.T) {
	expected := 3
	actual := SolvePartTwo(example)

	if actual != expected {
		t.Errorf(`Part 2: Expected: "%d" Actual: "%d"`, expected, actual)
	}
}
