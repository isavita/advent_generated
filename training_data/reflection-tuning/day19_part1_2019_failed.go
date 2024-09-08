package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func runIntcode(program []int, input []int) int {
	// Implement the Intcode computer here
	// This is a simplified version, you'll need to implement the full Intcode computer
	// Return the last output
	return 0
}

func main() {
	// Read the Intcode program
	content, err := ioutil.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}

	programStr := strings.Split(strings.TrimSpace(string(content)), ",")
	program := make([]int, len(programStr))
	for i, s := range programStr {
		program[i], _ = strconv.Atoi(s)
	}

	affected := 0
	for y := 0; y < 50; y++ {
		for x := 0; x < 50; x++ {
			output := runIntcode(program, []int{x, y})
			if output == 1 {
				affected++
			}
		}
	}

	fmt.Printf("Number of points affected by the tractor beam: %d\n", affected)
}
