package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strings"
)

const (
	initialAP = 3
	initialHP = 200
)

func abs(x int) int {
	if x > 0 {
		return x
	}
	return -x
}

type unit struct {
	id   int
	kind rune
	hp   int
	ap   int
	pos  point
}

type tiles map[point]rune

type units []*unit

type point struct {
	x int
	y int
}

func (p point) distance(q point) int {
	return abs(p.y-q.y) + abs(p.x-q.x)
}

func (p point) adjacent() []point {
	return []point{
		point{p.x, p.y - 1},
		point{p.x - 1, p.y},
		point{p.x + 1, p.y},
		point{p.x, p.y + 1},
	}
}

// Implement sort for reading order on units
func (u units) Less(i, j int) bool {
	a, b := u[i].pos, u[j].pos
	return (a.y < b.y) || (a.y == b.y && a.x < b.x)
}

func (u units) Swap(i, j int) {
	u[i], u[j] = u[j], u[i]
}

func (u units) Len() int {
	return len(u)
}

type stage struct {
	tiles  tiles
	units  units
	width  int
	height int
	rounds int
}

func (s *stage) score() int {
	hp := 0
	for _, unit := range s.units {
		hp += unit.hp
	}
	return hp
}

func (s *stage) findPaths(start point) map[point]path {
	nodes := map[point]bool{}

	for pos, tile := range s.tiles {
		nodes[pos] = (tile == '#')
	}

	for _, v := range s.units {
		nodes[v.pos] = true
	}

	return shortestPaths(start, nodes)
}

func (s *stage) remove(u *unit) {
	i := -1
	for j, v := range s.units {
		if v == u {
			i = j
		}
	}
	s.units = append(s.units[:i], s.units[i+1:]...)
}

func (s *stage) simulate() int {
	for round := 0; ; round++ {
		fmt.Printf("\nAfter %d rounds (%d units left):\n", round, len(s.units))
		s.debug()
		sort.Stable(s.units)

		// Can't range over keys, because that breaks when we remove a dead unit
		for i := 0; i < len(s.units); i++ {
			u := s.units[i]
			targets := []*unit{}

			for _, v := range s.units {
				if v.kind != u.kind {
					targets = append(targets, v)
				}
			}

			// If there are no more targets, then one side has won
			if len(targets) == 0 {
				return round
			}

			// Check for a target in range of the current unit
			canAttack := false
			squares := []point{}

			for _, v := range targets {
				if u.pos.distance(v.pos) == 1 {
					canAttack = true
					break
				}

				squares = append(squares, v.pos.adjacent()...)
			}

			if canAttack == false {
				// Find shortest path to one of the squares adjacent to a target
				shortest := []point{}
				paths := s.findPaths(u.pos)

				for _, pos := range squares {
					if path, ok := paths[pos]; ok {
						if len(path) < len(shortest) || len(shortest) == 0 {
							shortest = path
						}
					}
				}

				// Move the unit one step along the selected path
				if len(shortest) > 0 {
					u.pos = shortest[0]
				}
			}

			var target *unit

			// Attack first target within range
			for _, v := range targets {
				if u.pos.distance(v.pos) > 1 {
					continue
				}

				if target == nil || v.hp < target.hp {
					target = v
				}
			}

			if target != nil {
				//fmt.Printf(" - %d hit %d\n", u.id, target.id)
				target.hp -= u.ap

				if target.hp <= 0 {
					s.remove(target)
				}
			}
		}
	}
}

func (s *stage) debug() {
	index := map[point]*unit{}

	for _, u := range s.units {
		index[u.pos] = u
	}

	for y := 0; y < s.width; y++ {
		row := []*unit{}

		for x := 0; x <= s.height; x++ {
			pos := point{x, y}

			if unit, ok := index[pos]; ok {
				row = append(row, unit)
				fmt.Printf("%c", unit.kind)
				continue
			}

			if tile, ok := s.tiles[pos]; ok {
				fmt.Printf(string(tile))
			}
		}

		for _, unit := range row {
			fmt.Printf(" %c(%d)", unit.kind, unit.hp)
		}

		fmt.Printf("\n")
	}
}

func parse(input string) stage {
	tiles := tiles{}
	units := units{}
	width, height := 0, 0

	for y, line := range strings.Split(input, "\n") {
		for x, char := range line {
			pos := point{x, y}

			if char == 'E' || char == 'G' {
				units = append(units, &unit{
					id:   len(units),
					kind: char,
					pos:  pos,
					hp:   initialHP,
					ap:   initialAP,
				})
				char = '.'
			}

			tiles[pos] = char

			if x+1 >= width {
				width = x + 1
			}
		}

		if y+1 >= height {
			height = y + 1
		}
	}

	return stage{
		tiles:  tiles,
		units:  units,
		width:  width,
		height: height,
		rounds: 0,
	}
}

func solvePartOne(input string) int {
	stage := parse(input)
	round := stage.simulate()
	return stage.score() * round
}

func solvePartTwo(input string) int {
	return 0
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := string(bytes)
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input))
}
