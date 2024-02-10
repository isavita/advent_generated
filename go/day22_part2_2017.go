package main

import (
	"bufio"
	"fmt"
	"os"
)

type position struct {
	x, y int
}

const (
	Clean = iota
	Weakened
	Infected
	Flagged
)

func main() {
	file, _ := os.Open("input.txt")
	scanner := bufio.NewScanner(file)

	grid := make(map[position]int) // Clean = 0, Weakened = 1, Infected = 2, Flagged = 3
	var startX, startY int
	for y := 0; scanner.Scan(); y++ {
		line := scanner.Text()
		for x, c := range line {
			if c == '#' {
				grid[position{x, y}] = Infected
			}
		}
		startX, startY = len(line)/2, y/2
	}

	dx := []int{0, 1, 0, -1}
	dy := []int{-1, 0, 1, 0}

	x, y, dir := startX, startY, 0 // Start facing up
	infectedCount := 0

	for i := 0; i < 10000000; i++ {
		pos := position{x, y}
		switch grid[pos] {
		case Clean:
			dir = (dir - 1 + 4) % 4
			grid[pos] = Weakened
		case Weakened:
			grid[pos] = Infected
			infectedCount++
		case Infected:
			dir = (dir + 1) % 4
			grid[pos] = Flagged
		case Flagged:
			dir = (dir + 2) % 4
			grid[pos] = Clean
		}
		x, y = x+dx[dir], y+dy[dir]
	}

	fmt.Println(infectedCount)
}