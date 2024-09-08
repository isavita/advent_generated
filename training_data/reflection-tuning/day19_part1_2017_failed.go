package main

import (
	"bufio"
	"fmt"
	"os"
)

type Point struct {
	x, y int
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	var grid []string
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}

	start := Point{x: -1, y: 0}
	for x, ch := range grid[0] {
		if ch == '|' {
			start.x = x
			break
		}
	}

	if start.x == -1 {
		fmt.Println("No starting point found")
		return
	}

	dir := Point{x: 0, y: 1}
	current := start
	var path string

	for {
		current.x += dir.x
		current.y += dir.y

		if current.y < 0 || current.y >= len(grid) || current.x < 0 || current.x >= len(grid[current.y]) {
			break
		}

		ch := grid[current.y][current.x]

		if ch >= 'A' && ch <= 'Z' {
			path += string(ch)
		} else if ch == '+' {
			if dir.x == 0 {
				if current.x > 0 && (grid[current.y][current.x-1] == '-' || (grid[current.y][current.x-1] >= 'A' && grid[current.y][current.x-1] <= 'Z')) {
					dir = Point{x: -1, y: 0}
				} else if current.x < len(grid[current.y])-1 && (grid[current.y][current.x+1] == '-' || (grid[current.y][current.x+1] >= 'A' && grid[current.y][current.x+1] <= 'Z')) {
					dir = Point{x: 1, y: 0}
				} else {
					break
				}
			} else {
				if current.y > 0 && (grid[current.y-1][current.x] == '|' || (grid[current.y-1][current.x] >= 'A' && grid[current.y-1][current.x] <= 'Z')) {
					dir = Point{x: 0, y: -1}
				} else if current.y < len(grid)-1 && (grid[current.y+1][current.x] == '|' || (grid[current.y+1][current.x] >= 'A' && grid[current.y+1][current.x] <= 'Z')) {
					dir = Point{x: 0, y: 1}
				} else {
					break
				}
			}
		} else if ch == ' ' {
			break
		}
	}

	fmt.Println(path)
}
