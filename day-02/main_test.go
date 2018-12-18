package main

import "testing"

const example1 = `abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab`

const example2 = `abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz`

func TestSolvePartOne(t *testing.T) {
	expected := 12
	actual := solvePartOne(example1)

	if actual != expected {
		t.Errorf(`Part 1: Expected: %d Actual: %d`, expected, actual)
	}
}

func TestSolvePartTwo(t *testing.T) {
	expected := "fgij"
	actual := solvePartTwo(example2)

	if actual != expected {
		t.Errorf(`Part 2: Expected: "%s" Actual: "%s"`, expected, actual)
	}
}
