package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

func solvePartOne(input string) int {
	ids := strings.Split(input, "\n")

	twos := 0
	threes := 0

	for _, id := range ids {
		buckets := map[rune]int{}

		for _, r := range id {
			buckets[r] += 1
		}

		for _, n := range buckets {
			if n == 2 {
				twos++
				break
			}
		}

		for _, n := range buckets {
			if n == 3 {
				threes++
				break
			}
		}
	}

	return twos * threes
}

func solvePartTwo(input string) string {
	ids := strings.Split(input, "\n")

	for _, a := range ids {
		for _, b := range ids {
			if a == b {
				continue
			}

			diff := 0
			mark := 0

			for i := range a {
				if a[i] != b[i] {
					diff += 1
					mark = i
				}
			}

			if diff == 1 {
				return a[:mark] + b[mark+1:]
			}
		}
	}

	return ""
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input))
}
