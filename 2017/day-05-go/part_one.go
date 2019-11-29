// http://adventofcode.com/2017/day/5

package main

import "fmt"

type Program struct {
  steps int
  index int
  jumps []int
}

func Exec(program Program) Program {
  jump := program.jumps[program.index]
  program.jumps[program.index] += 1

  return Program {
    steps: program.steps + 1,
    index: program.index + jump,
    jumps: program.jumps,
  }
}

func IsRunning(program Program) bool {
  return program.index >= 0 && program.index < len(program.jumps)
}

func Solve([]int jumps) {
  program := Program { jumps: jumps }

  for IsRunning(program) {
    program = Exec(program)
  }

  return program.steps
}
