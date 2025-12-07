package main

import (
	"bufio"
	"fmt"
	"log"
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
		fmt.Println("Empty grid")
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

	// Active beams set: map[x_coordinate] -> exists
	activeBeams := make(map[int]bool)
	activeBeams[startX] = true

	totalSplits := 0

	// Simulate row by row starting from the row S is on
	// Beams move downward, so we process row i, and populate row i+1 (logically)
	// Actually, the logic is: beam enters row i at column x. 
	// If grid[i][x] is splitter, split. Else continue.
	// The results become the input for row i+1.
	for y := startY; y < height; y++ {
		nextBeams := make(map[int]bool)
		
		for x := range activeBeams {
			// Bounds check (just in case beams went out of bounds previously, 
			// though we handle that on insertion)
			if x < 0 || x >= width {
				continue
			}

			cell := grid[y][x]

			if cell == '^' {
				// Splitter: increment count, create new beams left and right
				totalSplits++
				
				// Left beam
				if x-1 >= 0 {
					nextBeams[x-1] = true
				}
				// Right beam
				if x+1 < width {
					nextBeams[x+1] = true
				}
			} else {
				// Empty space ('.') or Start ('S'): beam continues straight
				nextBeams[x] = true
			}
		}

		// Advance to next row
		activeBeams = nextBeams
		
		// Optimization: if no beams left, stop early
		if len(activeBeams) == 0 {
			break
		}
	}

	fmt.Printf("Total times the beam is split: %d\n", totalSplits)
}
