package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

func solvePartOne(input string) int {
	nums := strings.Split(input, "\n")
	freq := 0

	for _, num := range nums {
		f, _ := strconv.Atoi(num)
		freq += f
	}

	return freq
}

func solvePartTwo(input string) int {
	nums := strings.Split(input, "\n")
	seen := map[int]bool{}
	freq := 0

	for {
		for _, num := range nums {
			if seen[freq] == true {
				return freq
			} else {
				seen[freq] = true
			}

			f, _ := strconv.Atoi(num)
			freq += f
		}
	}
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input))
}
