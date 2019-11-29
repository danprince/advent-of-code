package main

import "testing"

const example = `2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2`

func TestSolvePartOne(t *testing.T) {
	actual := solvePartOne(example)
	expected := 138

	if actual != expected {
		t.Errorf(`Part 1: Expected: %#v Actual: %#v`, expected, actual)
	}
}

func TestSolvePartTwo(t *testing.T) {
	actual := solvePartTwo(example)
	expected := 66

	if actual != expected {
		t.Errorf(`Part 2: Expected: %#v Actual: %#v`, expected, actual)
	}
}
