package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

type pattern [5]bool
type rules map[pattern]bool

type state struct {
	pots map[int]bool
	min  int
	max  int
}

func (state *state) score() int {
	score := 0
	for index := range state.pots {
		score += index
	}
	return score
}

func (s *state) next(rules rules) state {
	next := state{
		pots: map[int]bool{},
		min:  s.min,
		max:  s.max,
	}

	for i := s.min - 2; i < s.max+3; i++ {
		output, exists := rules[pattern{
			s.pots[i-2],
			s.pots[i-1],
			s.pots[i+0],
			s.pots[i+1],
			s.pots[i+2],
		}]

		if exists && output {
			next.pots[i] = true
			if i < s.min {
				next.min = i
			}
			if i > s.max {
				next.max = i
			}
		}
	}

	return next
}

func parse(input string) (state, rules) {
	lines := strings.Split(input, "\n")
	state := state{pots: map[int]bool{}}
	rules := rules{}

	for i, char := range lines[0][15:] {
		if char == '#' {
			state.pots[i] = true
			state.max = i + 1
		}
	}

	for _, line := range lines[2:] {
		pattern := pattern{
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

func solvePartOne(input string) int {
	state, rules := parse(input)

	for gen := 0; gen < 20; gen++ {
		state = state.next(rules)
	}

	return state.score()
}

func solvePartTwo(input string) int {
	generations := 50000000000
	state, rules := parse(input)
	var gen, run, lastScore, lastDiff int

	for gen = 0; gen < generations; gen++ {
		state = state.next(rules)
		score := state.score()
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

	return state.score() + (generations-gen-1)*lastDiff
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input))
}
