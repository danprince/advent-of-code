package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strings"
)

type adjacencyList map[string][]string
type sortedStack []string

func (stack sortedStack) push(item string) sortedStack {
	stack = append(stack, item)
	sort.Strings(stack)
	return stack
}

func (stack sortedStack) pop() (string, sortedStack) {
	return stack[0], stack[1:]
}

func parse(input string) adjacencyList {
	graph := adjacencyList{}

	for _, line := range strings.Split(input, "\n") {
		source, target := string(line[5]), string(line[36])
		graph[source] = append(graph[source], target)
		graph[target] = append(graph[target])
	}

	return graph
}

func sumDependencies(graph adjacencyList) map[string]int {
	deps := map[string]int{}

	for parent, children := range graph {
		deps[parent] += 0

		for _, child := range children {
			deps[child] += 1
		}
	}

	return deps
}

func getNodeTime(node string) int {
	return int(node[0] - 64)
}

func solvePartOne(input string) string {
	graph := parse(input)
	deps := sumDependencies(graph)
	stack := sortedStack{}
	done := []string{}

	for node := range deps {
		if deps[node] == 0 {
			stack = stack.push(node)
		}
	}

	var node, child string

	for len(stack) > 0 {
		node, stack = stack.pop()
		done = append(done, node)

		for _, child = range graph[node] {
			deps[child] -= 1

			if deps[child] == 0 {
				stack = stack.push(child)
			}
		}
	}

	return strings.Join(done, "")
}

func solvePartTwo(input string, workers, delay int) int {
	graph := parse(input)
	deps := sumDependencies(graph)
	waiting := sortedStack{}
	tasks := map[int]string{}
	timers := map[int]int{}
	done := []string{}
	time := 0

	for node := range deps {
		if deps[node] == 0 {
			waiting = waiting.push(node)
		}
	}

	for {
		// Process tasks in active workers
		for worker, node := range tasks {
			timers[worker] -= 1

			if timers[worker] == 0 {
				delete(tasks, worker)
				done = append(done, node)

				for _, child := range graph[node] {
					deps[child] -= 1

					if deps[child] == 0 {
						waiting = waiting.push(child)
					}
				}
			}
		}

		if len(done) == len(graph) {
			return time
		} else {
			time += 1
		}

		blocked := sortedStack{}

		// Assign waiting nodes to available workers
		for _, node := range waiting {
			assigned := false

			for worker := 0; worker < workers; worker++ {
				if timers[worker] == 0 {
					tasks[worker] = node
					timers[worker] = getNodeTime(node) + delay
					assigned = true
					break
				}
			}

			if !assigned {
				blocked = blocked.push(node)
			}
		}

		// Put blocked nodes back into waiting list
		waiting = blocked
	}
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input, 5, 60))
}
