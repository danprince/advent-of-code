package main

import (
	"container/ring"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

func play(players, turns int) int {
	scores := map[int]int{}
	circle := ring.New(1)
	circle.Value = 0

	for turn := 0; turn <= turns; turn++ {
		player := turn % players

		if turn%23 == 0 {
			circle = circle.Move(-8)
			marble := circle.Unlink(1)
			circle = circle.Move(1)
			scores[player] += turn + marble.Value.(int)
		} else {
			marble := ring.New(1)
			marble.Value = turn
			circle = circle.Move(1).Link(marble).Move(-1)
		}
	}

	highscore := 0

	for _, score := range scores {
		if score > highscore {
			highscore = score
		}
	}

	return highscore
}

func parse(input string) (int, int) {
	parts := strings.Split(input, " ")
	players, _ := strconv.Atoi(parts[0])
	limit, _ := strconv.Atoi(parts[6])
	return players, limit
}

func solvePartOne(input string) int {
	players, limit := parse(input)
	return play(players, limit)
}

func solvePartTwo(input string) int {
	players, limit := parse(input)
	return play(players, limit*100)
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := string(bytes)
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input))
}
