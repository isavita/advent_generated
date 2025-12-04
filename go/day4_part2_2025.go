package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

// Point helps track coordinates to remove
type Point struct {
	r, c int
}

func main() {
	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// Read grid into a mutable 2D byte slice
	var grid [][]byte
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line != "" {
			grid = append(grid, []byte(line))
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	if len(grid) == 0 {
		fmt.Println("Total rolls removed: 0")
		return
	}

	rows := len(grid)
	cols := len(grid[0])
	totalRemoved := 0

	// Simulation Loop
	for {
		var toRemove []Point

		// 1. Identification Phase
		// Find all rolls that meet the criteria based on the CURRENT grid state
		for r := 0; r < rows; r++ {
			for c := 0; c < cols; c++ {
				// We only care about existing paper rolls
				if grid[r][c] != '@' {
					continue
				}

				// Count neighbors that are also paper rolls
				neighbors := 0
				for dr := -1; dr <= 1; dr++ {
					for dc := -1; dc <= 1; dc++ {
						if dr == 0 && dc == 0 {
							continue
						}
						nr, nc := r+dr, c+dc

						// Check bounds
						if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
							if grid[nr][nc] == '@' {
								neighbors++
							}
						}
					}
				}

				// If fewer than 4 neighbors, mark for removal
				if neighbors < 4 {
					toRemove = append(toRemove, Point{r, c})
				}
			}
		}

		// 2. Update Phase
		// If nothing to remove, we are done
		if len(toRemove) == 0 {
			break
		}

		// Apply removals
		totalRemoved += len(toRemove)
		for _, p := range toRemove {
			// Change '@' to '.' so it is no longer counted as a neighbor
			grid[p.r][p.c] = '.'
		}
	}

	fmt.Printf("Total rolls removed: %d\n", totalRemoved)
}
