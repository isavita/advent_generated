package main

import (
	"bufio"
	"fmt"
	"log"
	"math/big"
	"os"
)

func main() {
	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// Read the grid
	var grid []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line != "" {
			grid = append(grid, line)
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	if len(grid) == 0 {
		fmt.Println("0")
		return
	}

	height := len(grid)
	width := len(grid[0])

	// Find Starting Position 'S'
	var startX, startY int
	found := false
	for y, row := range grid {
		for x, char := range row {
			if char == 'S' {
				startX = x
				startY = y
				found = true
				break
			}
		}
		if found {
			break
		}
	}

	if !found {
		log.Fatal("Start point 'S' not found")
	}

	// Map of column index -> count of timelines (using big.Int)
	// We use a map because particles can go out of bounds (negative x or x >= width)
	counts := make(map[int]*big.Int)
	counts[startX] = big.NewInt(1)

	// Simulate row by row
	for y := startY; y < height; y++ {
		nextCounts := make(map[int]*big.Int)

		for x, count := range counts {
			// Determine what is at grid[y][x]
			// If out of bounds, treat as empty space '.'
			isSplitter := false
			if x >= 0 && x < width {
				if grid[y][x] == '^' {
					isSplitter = true
				}
			}

			if isSplitter {
				// Split into left (x-1) and right (x+1)
				// Add 'count' to nextCounts[x-1]
				addBig(nextCounts, x-1, count)
				// Add 'count' to nextCounts[x+1]
				addBig(nextCounts, x+1, count)
			} else {
				// Continue straight down to x
				addBig(nextCounts, x, count)
			}
		}

		counts = nextCounts
	}

	// Sum all final counts
	totalTimelines := new(big.Int)
	for _, count := range counts {
		totalTimelines.Add(totalTimelines, count)
	}

	fmt.Printf("Total different timelines: %s\n", totalTimelines.String())
}

// Helper to add value to the map
func addBig(m map[int]*big.Int, key int, val *big.Int) {
	if _, exists := m[key]; !exists {
		m[key] = new(big.Int)
	}
	m[key].Add(m[key], val)
}
