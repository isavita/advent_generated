package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	// Read input from file
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var grid [][]rune
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		grid = append(grid, []rune(scanner.Text()))
	}

	rows, cols := len(grid), len(grid[0])
	totalLoad := 0

	// Process each column
	for col := 0; col < cols; col++ {
		nextEmptyRow := 0
		for row := 0; row < rows; row++ {
			switch grid[row][col] {
			case 'O':
				totalLoad += rows - nextEmptyRow
				nextEmptyRow++
			case '#':
				nextEmptyRow = row + 1
			}
		}
	}

	fmt.Println(totalLoad)
}
