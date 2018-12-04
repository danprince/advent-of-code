package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func SolvePartOne(input string) int {
	return 0
}

func SolvePartTwo(input string) int {
	return 0
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := string(bytes)
	fmt.Println("Part 1:", SolvePartOne(input))
	fmt.Println("Part 2:", SolvePartTwo(input))
}
