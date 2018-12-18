package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

const required = 10

func solvePartOne(input string) string {
	loops, _ := strconv.Atoi(input)
	scores := []int{3, 7}
	elves := []int{0, 1}

	for {
		n := 0
		for _, k := range elves {
			n += scores[k]
		}
		for _, r := range strconv.Itoa(n) {
			x, _ := strconv.Atoi(string(r))
			scores = append(scores, x)
		}
		for i, k := range elves {
			elves[i] = (elves[i] + scores[k] + 1) % len(scores)
		}
		if len(scores) >= loops+required {
			break
		}
	}

	output := ""
	start := loops
	end := start + required

	for _, x := range scores[start:end] {
		output += strconv.Itoa(x)
	}

	return output
}

func solvePartTwo(input string) string {
	search := []int{}
	scores := []int{3, 7}
	elves := []int{0, 1}

	for _, r := range input {
		x, _ := strconv.Atoi(string(r))
		search = append(search, x)
	}

	for {
		n := 0
		for _, k := range elves {
			n += scores[k]
		}

		for _, r := range strconv.Itoa(n) {
			x, _ := strconv.Atoi(string(r))
			scores = append(scores, x)

			if len(scores) < len(search) {
				continue
			}

			found := true
			tail := scores[len(scores)-len(search):]

			for i := 0; i < len(search); i++ {
				if search[i] != tail[i] {
					found = false
					break
				}
			}

			if found {
				return fmt.Sprintf("%d", len(scores)-len(search))
			}
		}

		for i, k := range elves {
			elves[i] = (elves[i] + scores[k] + 1) % len(scores)
		}
	}
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input))
}
