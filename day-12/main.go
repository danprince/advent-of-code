package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

type Pattern [5]bool
type Rules map[Pattern]bool

type State struct {
	Pots map[int]bool
	min  int
	max  int
}

func (state *State) Score() int {
	score := 0
	for index := range state.Pots {
		score += index
	}
	return score
}

func (state *State) Next(rules Rules) State {
	next := State{
		Pots: map[int]bool{},
		min:  state.min,
		max:  state.max,
	}

	for i := state.min - 2; i < state.max+3; i++ {
		output, exists := rules[Pattern{
			state.Pots[i-2],
			state.Pots[i-1],
			state.Pots[i+0],
			state.Pots[i+1],
			state.Pots[i+2],
		}]

		if exists && output {
			next.Pots[i] = true
			if i < state.min {
				next.min = i
			}
			if i > state.max {
				next.max = i
			}
		}
	}

	return next
}

func ParseInput(input string) (State, Rules) {
	lines := strings.Split(input, "\n")
	state := State{Pots: map[int]bool{}}
	rules := Rules{}

	for i, char := range lines[0][15:] {
		if char == '#' {
			state.Pots[i] = true
			state.max = i + 1
		}
	}

	for _, line := range lines[2:] {
		pattern := Pattern{
			line[0] == '#',
			line[1] == '#',
			line[2] == '#',
			line[3] == '#',
			line[4] == '#',
		}
		rules[pattern] = (line[9] == '#')
	}

	return state, rules
}

func SolvePartOne(input string) int {
	state, rules := ParseInput(input)

	for gen := 0; gen < 20; gen++ {
		state = state.Next(rules)
	}

	return state.Score()
}

func SolvePartTwo(input string) int {
	generations := 50000000000
	state, rules := ParseInput(input)
	var gen, run, lastScore, lastDiff int

	for gen = 0; gen < generations; gen++ {
		state = state.Next(rules)
		score := state.Score()
		diff := score - lastScore

		if diff == lastDiff {
			run += 1
		} else {
			run = 0
		}

		if run > 5 {
			break
		}

		lastScore, lastDiff = score, diff
	}

	return state.Score() + (generations-gen-1)*lastDiff
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", SolvePartOne(input))
	fmt.Println("Part 2:", SolvePartTwo(input))
}
