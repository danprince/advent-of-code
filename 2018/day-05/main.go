package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strings"
	"unicode"
)

func react(polymer []rune) []rune {
	stack := []rune{}

	for _, b := range polymer {
		if len(stack) == 0 {
			stack = append(stack, b)
			continue
		}

		a := stack[len(stack)-1]
		stack = stack[:len(stack)-1]

		if a == b || unicode.ToUpper(a) != unicode.ToUpper(b) {
			stack = append(stack, a, b)
		}
	}

	return stack
}

func solvePartOne(input string) int {
	polymer := react([]rune(input))
	return len(polymer)
}

func solvePartTwo(input string) int {
	polymer := []rune(input)
	runes := map[rune]bool{}

	for _, r := range polymer {
		runes[unicode.ToUpper(r)] = true
	}

	score := len(input)

	for r := range runes {
		regex := regexp.MustCompile("(?i)" + string(r))
		str := regex.ReplaceAllString(input, "")
		polymer := react([]rune(str))

		if len(polymer) < score {
			score = len(polymer)
		}
	}

	return score
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input))
}
