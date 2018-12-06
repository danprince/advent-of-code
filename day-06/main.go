package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	X int
	Y int
}

type Grid struct {
	X0     int
	Y0     int
	X1     int
	Y1     int
	Points map[int]Point
}

// Manhattan Distance
func Distance(x0, y0, x1, y1 int) int {
	dx := float64(x1 - x0)
	dy := float64(y1 - y0)
	return int(math.Abs(dx) + math.Abs(dy))
}

func ParseGrid(input string) Grid {
	minX := math.Inf(+1)
	minY := math.Inf(+1)
	maxX := 0.0
	maxY := 0.0
	points := map[int]Point{}

	for index, line := range strings.Split(input, "\n") {
		parts := strings.Split(line, ", ")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		points[index] = Point{X: x, Y: y}

		minX = math.Min(minX, float64(x))
		minY = math.Min(minY, float64(y))
		maxX = math.Max(maxX, float64(x))
		maxY = math.Max(maxY, float64(y))
	}

	return Grid{
		Points: points,
		X0:     int(minX),
		Y0:     int(minY),
		X1:     int(maxX),
		Y1:     int(maxY),
	}
}

func SolvePartOne(input string) int {
	grid := ParseGrid(input)
	sizes := map[int]int{}

	for x := grid.X0; x < grid.X1; x++ {
		for y := grid.Y0; y < grid.Y1; y++ {
			min := math.MaxInt64
			shared := false
			region := 0

			for id, b := range grid.Points {
				dist := Distance(x, y, b.X, b.Y)

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

	for id, point := range grid.Points {
		// Region can't be finite when on grid boundaries
		if point.X == grid.X0 || point.Y == grid.Y0 || point.X == grid.X1 || point.Y == grid.Y1 {
			continue
		}

		if sizes[id] > size {
			size = sizes[id]
		}
	}

	return size
}

func SolvePartTwo(input string, distance int) int {
	grid := ParseGrid(input)
	size := 0

	for x := grid.X0; x < grid.X1; x++ {
		for y := grid.Y0; y < grid.Y1; y++ {
			total := 0

			for _, point := range grid.Points {
				total += Distance(x, y, point.X, point.Y)
			}

			if int(total) < distance {
				size += 1
			}
		}
	}

	return size
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", SolvePartOne(input))
	fmt.Println("Part 2:", SolvePartTwo(input, 10000))
	DrawPartOne(input, "part1.png")
	DrawPartTwo(input, 10000, "part2.png")
}
