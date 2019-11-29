package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func solvePartOne(input string) int {
	return 0
}

func solvePartTwo(input string) int {
	return 0
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := string(bytes)
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input))
}
