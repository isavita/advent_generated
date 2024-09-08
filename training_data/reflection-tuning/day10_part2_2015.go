package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func lookAndSay(input []byte) []byte {
	var result strings.Builder
	count := 1
	current := input[0]

	for i := 1; i < len(input); i++ {
		if input[i] == current {
			count++
		} else {
			result.WriteString(fmt.Sprintf("%d%c", count, current))
			count = 1
			current = input[i]
		}
	}
	result.WriteString(fmt.Sprintf("%d%c", count, current))

	return []byte(result.String())
}

func applyProcess(input []byte, times int) int {
	sequence := input
	for i := 0; i < times; i++ {
		sequence = lookAndSay(sequence)
	}
	return len(sequence)
}

func main() {
	input, err := ioutil.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input = []byte(strings.TrimSpace(string(input)))

	part1 := applyProcess(input, 40)
	part2 := applyProcess(input, 50)

	fmt.Printf("Part 1: %d\n", part1)
	fmt.Printf("Part 2: %d\n", part2)
}
