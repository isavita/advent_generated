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

	// Read the grid into memory
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

	rows := len(grid)
	if rows == 0 {
		fmt.Println("Empty grid")
		return
	}
	cols := len(grid[0])

	accessibleCount := 0

	// Iterate through every cell in the grid
	for y := 0; y < rows; y++ {
		for x := 0; x < cols; x++ {
			// We are only interested in paper rolls
			if grid[y][x] != '@' {
				continue
			}

			// Count adjacent paper rolls
			neighbors := countPaperNeighbors(grid, x, y, rows, cols)

			// The condition: fewer than 4 neighbors
			if neighbors < 4 {
				accessibleCount++
			}
		}
	}

	fmt.Printf("Number of accessible rolls of paper: %d\n", accessibleCount)
}

// countPaperNeighbors checks the 8 surrounding cells for '@'
func countPaperNeighbors(grid []string, x, y, rows, cols int) int {
	count := 0

	// Offsets for the 8 directions
	// (-1, -1) (0, -1) (1, -1)
	// (-1,  0)         (1,  0)
	// (-1,  1) (0,  1) (1,  1)
	for dy := -1; dy <= 1; dy++ {
		for dx := -1; dx <= 1; dx++ {
			// Skip the center cell itself
			if dx == 0 && dy == 0 {
				continue
			}

			nx, ny := x+dx, y+dy

			// Check boundaries
			if nx >= 0 && nx < cols && ny >= 0 && ny < rows {
				if grid[ny][nx] == '@' {
					count++
				}
			}
		}
	}
	return count
}
