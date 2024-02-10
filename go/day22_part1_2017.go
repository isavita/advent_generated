package main

import (
	"bufio"
	"fmt"
	"os"
)

type position struct {
	x, y int
}

func main() {
	file, _ := os.Open("input.txt")
	scanner := bufio.NewScanner(file)

	grid := make(map[position]bool) // true: infected, false: clean
	var startX, startY int
	for y := 0; scanner.Scan(); y++ {
		line := scanner.Text()
		for x, c := range line {
			if c == '#' {
				grid[position{x, y}] = true
			}
		}
		startX, startY = len(line)/2, y/2
	}

	// Directions: up, right, down, left
	dx := []int{0, 1, 0, -1}
	dy := []int{-1, 0, 1, 0}

	x, y, dir := startX, startY, 0 // Start facing up
	infectedCount := 0

	for i := 0; i < 10000; i++ {
		pos := position{x, y}
		if grid[pos] { // infected
			dir = (dir + 1) % 4 // Turn right
			delete(grid, pos)   // Clean
		} else { // clean
			dir = (dir - 1 + 4) % 4 // Turn left
			grid[pos] = true        // Infect
			infectedCount++
		}
		x, y = x+dx[dir], y+dy[dir] // Move forward
	}

	fmt.Println(infectedCount)
}