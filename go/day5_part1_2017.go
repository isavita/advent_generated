package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	// Step 1: Read Input
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}

	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	var offsets []int
	for _, line := range lines {
		offset, _ := strconv.Atoi(line)
		offsets = append(offsets, offset)
	}

	// Step 2: Initialize Variables
	index := 0
	steps := 0

	// Step 3: Navigate Maze
	for index >= 0 && index < len(offsets) {
		// Fetch the jump offset at the current index
		jump := offsets[index]

		// Step 4: Update Offset
		offsets[index]++

		// Move to the new index
		index += jump

		// Increment steps counter
		steps++
	}

	// Step 6: Output
	fmt.Println(steps)
}