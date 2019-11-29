package main

import (
	"math"
)

type path []point

// math.Inf becomes negative when converted to int, use max as stand-in value
var infinity = math.MaxInt32

func shortestPaths(start point, nodes map[point]bool) map[point]path {
	dist := map[point]int{}
	prev := map[point]point{}
	queue := map[point]bool{}

	for pos, blocked := range nodes {
		if !blocked {
			dist[pos] = infinity
			queue[pos] = true
		}
	}

	dist[start] = 0
	queue[start] = true

	for len(queue) > 0 {
		var u point

		for v := range queue {
			_, ok := dist[u]

			if dist[v] < dist[u] || !ok {
				u = v
			}
		}

		delete(queue, u)

		for _, v := range u.adjacent() {
			if dist[v] == 0 {
				continue
			}

			if alt := dist[u] + 1; alt < dist[v] {
				dist[v] = alt
				prev[v] = u
			}
		}
	}

	paths := map[point]path{}

	for end := range nodes {
		if dist[end] == infinity || dist[end] == 0 {
			continue
		}

		path := []point{end}
		current := end

		for {
			next, ok := prev[current]
			if ok && next != start {
				current = next
				path = append([]point{next}, path...)
			} else {
				break
			}
		}

		paths[end] = path
	}

	return paths
}
