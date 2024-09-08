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
	grid := readInput("input.txt")
	start := findStart(grid)
	loop, maxDist := findLoop(grid, start)
	enclosed := countEnclosed(grid, loop)

	fmt.Println("Part 1:", maxDist)
	fmt.Println("Part 2:", enclosed)
}

func readInput(filename string) []string {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var grid []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}
	return grid
}

func findStart(grid []string) Point {
	for y, row := range grid {
		for x, ch := range row {
			if ch == 'S' {
				return Point{x, y}
			}
		}
	}
	return Point{}
}

func findLoop(grid []string, start Point) (map[Point]bool, int) {
	loop := make(map[Point]bool)
	maxDist := 0
	var dfs func(p Point, dist int)
	dfs = func(p Point, dist int) {
		if p.x < 0 || p.y < 0 || p.y >= len(grid) || p.x >= len(grid[0]) {
			return
		}
		if loop[p] {
			return
		}
		loop[p] = true
		if dist > maxDist {
			maxDist = dist
		}
		ch := grid[p.y][p.x]
		if ch == '|' || ch == 'L' || ch == 'J' || ch == 'S' {
			dfs(Point{p.x, p.y - 1}, dist + 1)
		}
		if ch == '|' || ch == '7' || ch == 'F' || ch == 'S' {
			dfs(Point{p.x, p.y + 1}, dist + 1)
		}
		if ch == '-' || ch == 'L' || ch == 'F' || ch == 'S' {
			dfs(Point{p.x + 1, p.y}, dist + 1)
		}
		if ch == '-' || ch == 'J' || ch == '7' || ch == 'S' {
			dfs(Point{p.x - 1, p.y}, dist + 1)
		}
	}
	dfs(start, 0)
	return loop, maxDist
}

func countEnclosed(grid []string, loop map[Point]bool) int {
	count := 0
	for y, row := range grid {
		inside := false
		lastCorner := ' '
		for x, ch := range row {
			if loop[Point{x, y}] {
				switch ch {
				case '|':
					inside = !inside
				case 'F', 'L':
					lastCorner = ch
				case '7':
					if lastCorner == 'L' {
						inside = !inside
					}
					lastCorner = ' '
				case 'J':
					if lastCorner == 'F' {
						inside = !inside
					}
					lastCorner = ' '
				}
			} else if inside {
				count++
			}
		}
	}
	return count
}
