package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type Node struct {
	Index    int
	Width    int
	Children []Node
	Metadata []int
}

func (node Node) getValue() int {
	total := 0

	if len(node.Children) > 0 {
		for _, value := range node.Metadata {
			index := value - 1

			if index < len(node.Children) {
				total += node.Children[index].getValue()
			}
		}
	} else {
		for _, value := range node.Metadata {
			total += value
		}
	}

	return total
}

func ParseTree(input string) Node {
	data := []int{}

	for _, s := range strings.Split(input, " ") {
		num, _ := strconv.Atoi(s)
		data = append(data, num)
	}

	return ParseNode(data, 0)
}

func ParseNode(data []int, index int) Node {
	node := Node{Index: index}
	n, m := data[index], data[index+1]

	index += 2
	node.Width += 2

	for i := 0; i < n; i++ {
		child := ParseNode(data, index)
		node.Children = append(node.Children, child)
		node.Width += child.Width
		index += child.Width
	}

	node.Metadata = data[index : index+m]
	node.Width += m

	return node
}

func SolvePartOne(input string) int {
	tree := ParseTree(input)
	stack := []Node{tree}
	sum := 0

	for len(stack) > 0 {
		node := stack[0]
		stack = stack[1:]

		for _, value := range node.Metadata {
			sum += value
		}

		stack = append(stack, node.Children...)
	}

	return sum
}

func SolvePartTwo(input string) int {
	tree := ParseTree(input)
	return tree.getValue()
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", SolvePartOne(input))
	fmt.Println("Part 2:", SolvePartTwo(input))
}
