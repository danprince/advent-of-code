package main

import "testing"

const example = `1, 1
1, 6
8, 3
3, 4
5, 5
8, 9`

func TestSolvePartOne(t *testing.T) {
	actual := SolvePartOne(example)
	expected := 17

	if actual != expected {
		t.Errorf(`Part 1: Expected: %#v Actual: %#v`, expected, actual)
	}
}

func TestSolvePartTwo(t *testing.T) {
	actual := SolvePartTwo(example, 32)
	expected := 16

	if actual != expected {
		t.Errorf(`Part 2: Expected: %#v Actual: %#v`, expected, actual)
	}
}
