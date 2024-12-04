package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// checkMAS checks if "MAS" or "SAM" exists starting from position (x,y) in direction d
func checkMAS(grid []string, x, y int, dx, dy int) bool {
	if x < 0 || y < 0 || x >= len(grid) || y >= len(grid[0]) {
		return false
	}

	// Check for "MAS"
	forward := true
	backward := true
	word := "MAS"
	
	for i := 0; i < len(word); i++ {
		newX, newY := x+(dx*i), y+(dy*i)
		if newX < 0 || newY < 0 || newX >= len(grid) || newY >= len(grid[0]) {
			forward = false
			break
		}
		if grid[newX][newY] != word[i] {
			forward = false
		}
	}

	for i := 0; i < len(word); i++ {
		newX, newY := x+(dx*i), y+(dy*i)
		if newX < 0 || newY < 0 || newX >= len(grid) || newY >= len(grid[0]) {
			backward = false
			break
		}
		if grid[newX][newY] != word[len(word)-1-i] {
			backward = false
		}
	}

	return forward || backward
}

// checkXMAS checks if there's an X-MAS pattern centered at (x,y)
func checkXMAS(grid []string, x, y int) bool {
	// Check for X pattern in both possible orientations
	
	// First orientation: \ then /
	if checkMAS(grid, x-1, y-1, 1, 1) && // Upper-left to lower-right
		checkMAS(grid, x-1, y+1, 1, -1) { // Upper-right to lower-left
		return true
	}
	
	// Second orientation: / then \
	if checkMAS(grid, x+1, y-1, -1, 1) && // Lower-left to upper-right
		checkMAS(grid, x+1, y+1, -1, -1) { // Lower-right to upper-left
		return true
	}

	return false
}

// countXMASPatterns counts all X-MAS patterns in the grid
func countXMASPatterns(grid []string) int {
	count := 0
	
	// We need at least 3x3 grid for an X-MAS pattern
	if len(grid) < 3 || len(grid[0]) < 3 {
		return 0
	}

	// Check each possible center point of an X pattern
	for i := 1; i < len(grid)-1; i++ {
		for j := 1; j < len(grid[i])-1; j++ {
			if grid[i][j] == 'A' && checkXMAS(grid, i, j) {
				count++
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

	// Count X-MAS patterns
	count := countXMASPatterns(grid)
	fmt.Printf("X-MAS patterns appear %d times in the word search\n", count)
}
