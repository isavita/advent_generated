package main

import (
	"bufio"
	"fmt"
	"os"
)

const gridSize = 100
const steps = 100

func countOnNeighbors(grid [][]bool, x, y int) int {
	on := 0
	for dx := -1; dx <= 1; dx++ {
		for dy := -1; dy <= 1; dy++ {
			if dx == 0 && dy == 0 {
				continue
			}
			nx, ny := x+dx, y+dy
			if nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx][ny] {
				on++
			}
		}
	}
	return on
}

func step(grid [][]bool) [][]bool {
	newGrid := make([][]bool, gridSize)
	for i := range newGrid {
		newGrid[i] = make([]bool, gridSize)
	}

	for x := 0; x < gridSize; x++ {
		for y := 0; y < gridSize; y++ {
			onNeighbors := countOnNeighbors(grid, x, y)
			if grid[x][y] {
				newGrid[x][y] = onNeighbors == 2 || onNeighbors == 3
			} else {
				newGrid[x][y] = onNeighbors == 3
			}
		}
	}

	return newGrid
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	grid := make([][]bool, gridSize)
	for i := range grid {
		grid[i] = make([]bool, gridSize)
	}

	scanner := bufio.NewScanner(file)
	y := 0
	for scanner.Scan() {
		line := scanner.Text()
		for x, c := range line {
			grid[x][y] = c == '#'
		}
		y++
	}

	for i := 0; i < steps; i++ {
		grid = step(grid)
	}

	onCount := 0
	for _, row := range grid {
		for _, light := range row {
			if light {
				onCount++
			}
		}
	}

	fmt.Println(onCount)
}