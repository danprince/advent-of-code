package main

import "testing"

const example = ``

func TestSolvePartOne(t *testing.T) {
	expected := 0
	actual := SolvePartOne(example)

	if actual != expected {
		t.Errorf(`Part 1: Expected: %d Actual: %d`, expected, actual)
	}
}

func TestSolvePartTwo(t *testing.T) {
	expected := 0
	actual := SolvePartTwo(example)

	if actual != expected {
		t.Errorf(`Part 2: Expected: "%d" Actual: "%d"`, expected, actual)
	}
}
