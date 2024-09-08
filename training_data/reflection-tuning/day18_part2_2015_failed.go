package main

import (
	"bufio"
	"fmt"
	"os"
)

const gridSize = 100
const steps = 100

func main() {
	grid := make([][]bool, gridSize)
	for i := range grid {
		grid[i] = make([]bool, gridSize)
	}

	// Read input
	scanner := bufio.NewScanner(os.Stdin)
	for i := 0; i < gridSize && scanner.Scan(); i++ {
		line := scanner.Text()
		for j, ch := range line {
			grid[i][j] = ch == '#'
		}
	}

	// Part 1
	gridCopy := copyGrid(grid)
	for i := 0; i < steps; i++ {
		gridCopy = step(gridCopy, false)
	}
	fmt.Printf("Part 1: %d\n", countLights(gridCopy))

	// Part 2
	gridCopy = copyGrid(grid)
	setCorners(gridCopy)
	for i := 0; i < steps; i++ {
		gridCopy = step(gridCopy, true)
	}
	fmt.Printf("Part 2: %d\n", countLights(gridCopy))
}

func copyGrid(grid [][]bool) [][]bool {
	newGrid := make([][]bool, len(grid))
	for i := range grid {
		newGrid[i] = make([]bool, len(grid[i]))
		copy(newGrid[i], grid[i])
	}
	return newGrid
}

func setCorners(grid [][]bool) {
	grid[0][0] = true
	grid[0][gridSize-1] = true
	grid[gridSize-1][0] = true
	grid[gridSize-1][gridSize-1] = true
}

func step(grid [][]bool, cornersAlwaysOn bool) [][]bool {
	newGrid := make([][]bool, len(grid))
	for i := range newGrid {
		newGrid[i] = make([]bool, len(grid[i]))
	}

	for i := 0; i < gridSize; i++ {
		for j := 0; j < gridSize; j++ {
			count := countNeighbors(grid, i, j)
			if grid[i][j] {
				newGrid[i][j] = count == 2 || count == 3
			} else {
				newGrid[i][j] = count == 3
			}
		}
	}

	if cornersAlwaysOn {
		setCorners(newGrid)
	}

	return newGrid
}

func countNeighbors(grid [][]bool, x, y int) int {
	count := 0
	for i := -1; i <= 1; i++ {
		for j := -1; j <= 1; j++ {
			if i == 0 && j == 0 {
				continue
			}
			nx, ny := x+i, y+j
			if nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx][ny] {
				count++
			}
		}
	}
	return count
}

func countLights(grid [][]bool) int {
	count := 0
	for i := 0; i < gridSize; i++ {
		for j := 0; j < gridSize; j++ {
			if grid[i][j] {
				count++
			}
		}
	}
	return count
}
