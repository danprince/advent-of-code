package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type Node struct {
	Children []Node
	Metadata []int
}

func (node Node) Sum() int {
	total := 0

	for _, child := range node.Children {
		total += child.Sum()
	}

	for _, meta := range node.Metadata {
		total += meta
	}

	return total
}

func (node Node) Value() int {
	value := 0
	n := len(node.Children)

	if n > 0 {
		for _, meta := range node.Metadata {
			i := meta - 1

			if i < n {
				child := node.Children[i]
				value += child.Value()
			}
		}
	} else {
		for _, meta := range node.Metadata {
			value += meta
		}
	}

	return value
}

func ParseTree(input string) Node {
	data := []int{}

	for _, s := range strings.Split(input, " ") {
		num, _ := strconv.Atoi(s)
		data = append(data, num)
	}

	tree, _ := ParseNode(data)
	return tree
}

func ParseNode(data []int) (Node, []int) {
	n, m, data := data[0], data[1], data[2:]

	var node, child Node

	for i := 0; i < n; i++ {
		child, data = ParseNode(data)
		node.Children = append(node.Children, child)
	}

	node.Metadata = data[:m]
	data = data[m:]

	return node, data
}

func SolvePartOne(input string) int {
	return ParseTree(input).Sum()
}

func SolvePartTwo(input string) int {
	return ParseTree(input).Value()
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", SolvePartOne(input))
	fmt.Println("Part 2:", SolvePartTwo(input))
}
