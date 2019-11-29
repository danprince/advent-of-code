package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type node struct {
	children []node
	metadata []int
}

func (n node) sum() int {
	total := 0
	for _, child := range n.children {
		total += child.sum()
	}
	for _, meta := range n.metadata {
		total += meta
	}
	return total
}

func (n node) val() int {
	value := 0
	size := len(n.children)

	if size > 0 {
		for _, meta := range n.metadata {
			i := meta - 1

			if i < size {
				value += n.children[i].val()
			}
		}
	} else {
		for _, meta := range n.metadata {
			value += meta
		}
	}

	return value
}

func parseTree(input string) node {
	data := []int{}

	for _, s := range strings.Split(input, " ") {
		num, _ := strconv.Atoi(s)
		data = append(data, num)
	}

	tree, _ := parseNode(data)
	return tree
}

func parseNode(data []int) (node, []int) {
	n, m, data := data[0], data[1], data[2:]

	var node, child node

	for i := 0; i < n; i++ {
		child, data = parseNode(data)
		node.children = append(node.children, child)
	}

	node.metadata = data[:m]
	data = data[m:]

	return node, data
}

func solvePartOne(input string) int {
	return parseTree(input).sum()
}

func solvePartTwo(input string) int {
	return parseTree(input).val()
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input))
}
