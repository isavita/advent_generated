package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// Direction represents a direction to search in the grid
type Direction struct {
	dx, dy int
}

// getAllDirections returns all possible directions to search
func getAllDirections() []Direction {
	return []Direction{
		{0, 1},   // right
		{1, 0},   // down
		{1, 1},   // diagonal down-right
		{-1, 1},  // diagonal up-right
		{0, -1},  // left
		{-1, 0},  // up
		{-1, -1}, // diagonal up-left
		{1, -1},  // diagonal down-left
	}
}

// checkWord checks if the word exists starting from position (x,y) in direction d
func checkWord(grid []string, word string, x, y int, d Direction) bool {
	if x < 0 || y < 0 || x >= len(grid) || y >= len(grid[0]) {
		return false
	}

	for i := 0; i < len(word); i++ {
		newX, newY := x+(d.dx*i), y+(d.dy*i)
		if newX < 0 || newY < 0 || newX >= len(grid) || newY >= len(grid[0]) {
			return false
		}
		if grid[newX][newY] != word[i] {
			return false
		}
	}
	return true
}

// countOccurrences counts all occurrences of the word in the grid
func countOccurrences(grid []string, word string) int {
	count := 0
	directions := getAllDirections()

	for i := 0; i < len(grid); i++ {
		for j := 0; j < len(grid[i]); j++ {
			for _, dir := range directions {
				if checkWord(grid, word, i, j, dir) {
					count++
				}
			}
		}
	}
	return count
}

func main() {
	// Read input from file
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	// Read the grid
	var grid []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line != "" {
			grid = append(grid, line)
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	// Count occurrences of "XMAS"
	count := countOccurrences(grid, "XMAS")
	fmt.Printf("XMAS appears %d times in the word search\n", count)
}
