package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"os"
	"strconv"
	"strings"
)

type point struct {
	x int
	y int
}

type grid struct {
	x0     int
	y0     int
	x1     int
	y1     int
	points map[int]point
}

// Manhattan Distance
func distance(x0, y0, x1, y1 int) int {
	dx := float64(x1 - x0)
	dy := float64(y1 - y0)
	return int(math.Abs(dx) + math.Abs(dy))
}

func parse(input string) grid {
	minX := math.Inf(+1)
	minY := math.Inf(+1)
	maxX := 0.0
	maxY := 0.0
	points := map[int]point{}

	for index, line := range strings.Split(input, "\n") {
		parts := strings.Split(line, ", ")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		points[index] = point{x: x, y: y}

		minX = math.Min(minX, float64(x))
		minY = math.Min(minY, float64(y))
		maxX = math.Max(maxX, float64(x))
		maxY = math.Max(maxY, float64(y))
	}

	return grid{
		points: points,
		x0:     int(minX),
		y0:     int(minY),
		x1:     int(maxX),
		y1:     int(maxY),
	}
}

func solvePartOne(input string) int {
	grid := parse(input)
	sizes := map[int]int{}

	for x := grid.x0; x < grid.x1; x++ {
		for y := grid.y0; y < grid.y1; y++ {
			min := math.MaxInt64
			shared := false
			region := 0

			for id, b := range grid.points {
				dist := distance(x, y, b.x, b.y)

				if dist <= min {
					region = id
					shared = dist == min
					min = dist
				}
			}

			// If this cell has multiple nearest points, then it doesn't count
			// towards the region for any of them.
			if shared == false {
				sizes[region] += 1
			}
		}
	}

	size := 0

	for id, point := range grid.points {
		// Region can't be finite when on grid boundaries
		if point.x == grid.x0 || point.y == grid.y0 || point.x == grid.x1 || point.y == grid.y1 {
			continue
		}

		if sizes[id] > size {
			size = sizes[id]
		}
	}

	return size
}

func solvePartTwo(input string, dist int) int {
	grid := parse(input)
	size := 0

	for x := grid.x0; x < grid.x1; x++ {
		for y := grid.y0; y < grid.y1; y++ {
			total := 0

			for _, point := range grid.points {
				total += distance(x, y, point.x, point.y)
			}

			if int(total) < dist {
				size += 1
			}
		}
	}

	return size
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input, 10000))
	//DrawPartOne(input, "part1.png")
	//DrawPartTwo(input, 10000, "part2.png")
}
