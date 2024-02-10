package main

import (
	"fmt"
	"os"
	"strings"
)

type Network struct {
	Instructions string
	Nodes        map[string][2]string
}

func parseInput(input []string) Network {
	instructions := input[0]

	nodes := map[string][2]string{}
	for _, line := range input[2:] {
		head, children := parseLine(line)
		nodes[head] = children
	}

	network := Network{
		Instructions: instructions,
		Nodes:        nodes,
	}
	return network
}

func parseLine(line string) (head string, children [2]string) {
	parts := strings.Split(line, " = ")

	head = parts[0]
	childrenTrim := strings.Trim(parts[1], "()")
	childrenParts := strings.Split(childrenTrim, ", ")
	children = [2]string{childrenParts[0], childrenParts[1]}

	return head, children
}

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func lcm(a, b int) int {
	return (a * b) / gcd(a, b)
}

func lcmSlice(nums []int) int {
	if len(nums) == 0 {
		return 0
	}

	res := nums[0]
	for i := 1; i < len(nums); i++ {
		res = lcm(res, nums[i])
	}

	return res
}

func solve(input []string) int {
	network := parseInput(input)

	starts := []string{}
	for node := range network.Nodes {
		lastIndex := len(node) - 1
		if node[lastIndex] == 'A' {
			starts = append(starts, node)
		}
	}

	steps := make([]int, len(starts))
	instructionsLenght := len(network.Instructions)
	for i := 0; i < len(starts); i++ {
		element := starts[i]
		lastIndex := len(element) - 1
		for element[lastIndex] != 'Z' {
			instruction := network.Instructions[steps[i]%instructionsLenght]
			if instruction == 'L' {
				element = network.Nodes[element][0]
			} else {
				element = network.Nodes[element][1]
			}
			steps[i]++
		}
	}

	res := lcmSlice(steps)
	return res
}

func readFile(fileName string) []string {
	file, err := os.ReadFile(fileName)
	if err != nil {
		panic(err)
	}

	return strings.Split(strings.TrimSpace(string(file)), "\n")
}

func main() {
	input := readFile("input.txt")
	fmt.Println(solve(input))
}