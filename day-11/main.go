package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

func getPowerLevel(x, y, serial int) int {
	id := x + 10
	power := (id * y) + serial
	return ((power * id % 1000) / 100) - 5
}

type sumTable [][]int

func newSumTable(width, height int, sum func(x, y int) int) sumTable {
	table := make(sumTable, width)

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

func (table sumTable) Area(x0, y0, x1, y1 int) int {
	if x0 < 0 || y0 < 0 || x1 >= len(table) || y1 >= len(table[x1]) {
		return 0
	}

	return table[x1][y1] - table[x1][y0] - table[x0][y1] + table[x0][y0]
}

func solvePartOne(input string) string {
	serial, _ := strconv.Atoi(input)
	best, bx, by := 0, 0, 0

	sum := newSumTable(300, 300, func(x, y int) int {
		return getPowerLevel(x, y, serial)
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

func solvePartTwo(input string) string {
	serial, _ := strconv.Atoi(input)
	best, bs, bx, by := 0, 0, 0, 0

	sum := newSumTable(300, 300, func(x, y int) int {
		return getPowerLevel(x, y, serial)
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
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input))
}
