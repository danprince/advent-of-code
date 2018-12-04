package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type Point struct {
	x int
	y int
}

type Claim struct {
	id int
	x  int
	y  int
	w  int
	h  int
}

func ParseClaims(input string) []Claim {
	lines := strings.Split(input, "\n")
	r := regexp.MustCompile(`#(\d+) @ (\d+),(\d+): (\d+)x(\d+)`)

	claims := []Claim{}

	for _, line := range lines {
		matches := r.FindStringSubmatch(line)

		if matches == nil {
			continue
		}

		id, _ := strconv.Atoi(matches[1])
		x, _ := strconv.Atoi(matches[2])
		y, _ := strconv.Atoi(matches[3])
		w, _ := strconv.Atoi(matches[4])
		h, _ := strconv.Atoi(matches[5])

		claims = append(claims, Claim{id, x, y, w, h})
	}

	return claims
}

func PlotClaims(claims []Claim) map[Point][]int {
	claimed := map[Point][]int{}

	for _, claim := range claims {
		for x := claim.x; x < claim.x+claim.w; x++ {
			for y := claim.y; y < claim.y+claim.h; y++ {
				p := Point{x, y}

				if claimed[p] == nil {
					claimed[p] = []int{}
				}

				claimed[p] = append(claimed[p], claim.id)
			}
		}
	}

	return claimed
}

func SolvePartOne(input string) int {
	claims := ParseClaims(input)
	claimed := PlotClaims(claims)
	score := 0

	for _, ids := range claimed {
		if len(ids) >= 2 {
			score += 1
		}
	}

	return score
}

func SolvePartTwo(input string) int {
	claims := ParseClaims(input)
	claimed := PlotClaims(claims)

	overlaps := map[int]bool{}

	for _, claim := range claims {
		overlaps[claim.id] = false
	}

	for _, ids := range claimed {
		if len(ids) > 1 {
			for _, id := range ids {
				overlaps[id] = true
			}
		}
	}

	for id, overlap := range overlaps {
		if overlap == false {
			return id
		}
	}

	return -1
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := string(bytes)
	fmt.Println("Part 1:", SolvePartOne(input))
	fmt.Println("Part 2:", SolvePartTwo(input))
}
