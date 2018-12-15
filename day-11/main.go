package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

func PowerLevel(x, y, serial int) int {
	id := x + 10
	power := (id * y) + serial
	return ((power * id % 1000) / 100) - 5
}

type SumTable [][]int

func CreateTable(width, height int, sum func(x, y int) int) SumTable {
	table := make([][]int, width)

	for x := 0; x < width; x++ {
		table[x] = make([]int, height)
		for y := 0; y < height; y++ {
			total := sum(x, y)
			if y > 0 {
				total += table[x][y-1]
			}
			if x > 0 {
				total += table[x-1][y]
			}
			if y > 0 && x > 0 {
				total -= table[x-1][y-1]
			}
			table[x][y] = total
		}
	}

	return table
}

func (table SumTable) Area(x0, y0, x1, y1 int) int {
	if x0 < 0 || y0 < 0 || x1 >= len(table) || y1 >= len(table[x1]) {
		return 0
	}

	return table[x1][y1] - table[x1][y0] - table[x0][y1] + table[x0][y0]
}

func SolvePartOne(input string) string {
	serial, _ := strconv.Atoi(input)
	best, bx, by := 0, 0, 0

	sum := CreateTable(300, 300, func(x, y int) int {
		return PowerLevel(x, y, serial)
	})

	for x := 0; x < 300; x++ {
		for y := 0; y < 300; y++ {
			score := sum.Area(x, y, x+3, y+3)
			if score > best {
				best, bx, by = score, x, y
			}
		}
	}

	return fmt.Sprintf("%d,%d", bx+1, by+1)
}

func SolvePartTwo(input string) string {
	serial, _ := strconv.Atoi(input)
	best, bs, bx, by := 0, 0, 0, 0

	sum := CreateTable(300, 300, func(x, y int) int {
		return PowerLevel(x, y, serial)
	})

	for s := 0; s < 300; s++ {
		for x := 0; x < 300; x++ {
			for y := 0; y < 300; y++ {
				score := sum.Area(x, y, x+s, y+s)
				if score > best {
					best, bs, bx, by = score, s, x, y
				}
			}
		}
	}

	return fmt.Sprintf("%d,%d,%d", bx+1, by+1, bs)
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", SolvePartOne(input))
	fmt.Println("Part 2:", SolvePartTwo(input))
}
