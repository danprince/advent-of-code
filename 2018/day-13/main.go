package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strings"
)

const (
	left     = 0
	straight = 1
	right    = 2

	north = 0
	east  = 1
	south = 2
	west  = 3
)

type point struct {
	x int
	y int
}

type grid map[point]rune

func (g grid) at(x, y int) rune {
	return g[point{x, y}]
}

type cart struct {
	x    int
	y    int
	dir  int
	turn int
}

var turns = map[rune]func(*cart){
	'+': func(c *cart) {
		c.dir += (c.turn - 1)
		if c.dir < 0 {
			c.dir = 3
		} else if c.dir > 3 {
			c.dir = 0
		}
		c.turn = (c.turn + 1) % 3
	},
	'/': func(c *cart) {
		if c.dir == north {
			c.dir = east
		} else if c.dir == south {
			c.dir = west
		} else if c.dir == west {
			c.dir = south
		} else if c.dir == east {
			c.dir = north
		}
	},
	'\\': func(c *cart) {
		if c.dir == north {
			c.dir = west
		} else if c.dir == south {
			c.dir = east
		} else if c.dir == west {
			c.dir = north
		} else if c.dir == east {
			c.dir = south
		}
	},
}

func (c *cart) tick(track rune) {
	if turn, exists := turns[track]; exists {
		turn(c)
	}

	switch c.dir {
	case north:
		c.y -= 1
		break
	case south:
		c.y += 1
		break
	case west:
		c.x -= 1
		break
	case east:
		c.x += 1
		break
	default:
		panic("invalid direction")
	}
}

type carts []*cart

func (cs carts) Len() int {
	return len(cs)
}

func (cs carts) Less(i, j int) bool {
	I, J := cs[i], cs[j]
	if I.y == J.y {
		return I.x < J.x
	}
	return I.y < J.y
}

func (cs carts) Swap(i, j int) {
	cs[i], cs[j] = cs[j], cs[i]
}

func (cs carts) collides(c *cart) bool {
	for _, o := range cs {
		if c != o && c.x == o.x && c.y == o.y {
			return true
		}
	}
	return false
}

func (cs carts) remove(x, y int) carts {
	new := carts{}
	for _, c := range cs {
		if c.x != x || c.y != y {
			new = append(new, c)
		}
	}
	return new
}

func parseInput(input string) (grid, carts) {
	grid := grid{}
	carts := carts{}

	for y, line := range strings.Split(input, "\n") {
		for x, char := range line {
			point := point{x, y}

			if char == '^' {
				char, carts = '|', append(carts, &cart{x, y, north, left})
			} else if char == 'v' {
				char, carts = '|', append(carts, &cart{x, y, south, left})
			} else if char == '<' {
				char, carts = '-', append(carts, &cart{x, y, west, left})
			} else if char == '>' {
				char, carts = '-', append(carts, &cart{x, y, east, left})
			}

			grid[point] = char
		}
	}

	return grid, carts
}

func solvePartOne(input string) string {
	grid, carts := parseInput(input)

	for {
		sort.Sort(carts)

		for _, cart := range carts {
			track := grid.at(cart.x, cart.y)
			cart.tick(track)

			if carts.collides(cart) {
				return fmt.Sprintf("%d,%d", cart.x, cart.y)
			}
		}
	}
}

func solvePartTwo(input string) string {
	grid, carts := parseInput(input)

	for {
		sort.Sort(carts)

		for _, cart := range carts {
			track := grid.at(cart.x, cart.y)
			cart.tick(track)

			if carts.collides(cart) {
				carts = carts.remove(cart.x, cart.y)
			}
		}

		if len(carts) == 1 {
			return fmt.Sprintf("%d,%d", carts[0].x, carts[0].y)
		}
	}
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := string(bytes)
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input))
}
