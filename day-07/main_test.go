package main

import "testing"

const example = `Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.`

func TestSolvePartOne(t *testing.T) {
	actual := SolvePartOne(example)
	expected := "CABDFE"

	if actual != expected {
		t.Errorf(`Part 1: Expected: %#v Actual: %#v`, expected, actual)
	}
}

func TestSolvePartTwo(t *testing.T) {
	actual := SolvePartTwo(example, 2, 0)
	expected := 15

	if actual != expected {
		t.Errorf(`Part 2: Expected: %#v Actual: %#v`, expected, actual)
	}
}
