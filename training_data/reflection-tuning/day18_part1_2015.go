package main

import (
	"bufio"
	"fmt"
	"os"
)

const (
	gridSize = 100
	steps    = 100
)

func main() {
	grid := make([][]bool, gridSize+2)
	for i := range grid {
		grid[i] = make([]bool, gridSize+2)
	}

	// Read input
	file, _ := os.Open("input.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for i := 1; i <= gridSize && scanner.Scan(); i++ {
		for j, ch := range scanner.Text() {
			grid[i][j+1] = ch == '#'
		}
	}

	// Simulate steps
	for step := 0; step < steps; step++ {
		newGrid := make([][]bool, gridSize+2)
		for i := range newGrid {
			newGrid[i] = make([]bool, gridSize+2)
		}

		for i := 1; i <= gridSize; i++ {
			for j := 1; j <= gridSize; j++ {
				count := countNeighbors(grid, i, j)
				if grid[i][j] {
					newGrid[i][j] = count == 2 || count == 3
				} else {
					newGrid[i][j] = count == 3
				}
			}
		}
		grid = newGrid
	}

	// Count lights on
	lightsOn := 0
	for i := 1; i <= gridSize; i++ {
		for j := 1; j <= gridSize; j++ {
			if grid[i][j] {
				lightsOn++
			}
		}
	}

	fmt.Println(lightsOn)
}

func countNeighbors(grid [][]bool, i, j int) int {
	count := 0
	for di := -1; di <= 1; di++ {
		for dj := -1; dj <= 1; dj++ {
			if di == 0 && dj == 0 {
				continue
			}
			if grid[i+di][j+dj] {
				count++
			}
		}
	}
	return count
}
