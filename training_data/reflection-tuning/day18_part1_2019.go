package main

import (
	"bufio"
	"fmt"
	"os"
)

type Point struct {
	x, y int
}

type State struct {
	pos   Point
	keys  int
	steps int
}

func main() {
	grid, start, keys := parseInput("input.txt")
	fmt.Println(shortestPath(grid, start, keys))
}

func parseInput(filename string) ([][]byte, Point, int) {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)
	var grid [][]byte
	var start Point
	keys := 0
	for y := 0; scanner.Scan(); y++ {
		line := scanner.Text()
		grid = append(grid, []byte(line))
		for x, ch := range line {
			if ch == '@' {
				start = Point{x, y}
			} else if ch >= 'a' && ch <= 'z' {
				keys |= 1 << (ch - 'a')
			}
		}
	}
	return grid, start, keys
}

func shortestPath(grid [][]byte, start Point, allKeys int) int {
	queue := []State{{start, 0, 0}}
	visited := make(map[string]bool)
	dirs := []Point{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}

	for len(queue) > 0 {
		curr := queue[0]
		queue = queue[1:]

		if curr.keys == allKeys {
			return curr.steps
		}

		key := fmt.Sprintf("%d,%d,%d", curr.pos.x, curr.pos.y, curr.keys)
		if visited[key] {
			continue
		}
		visited[key] = true

		for _, dir := range dirs {
			nx, ny := curr.pos.x+dir.x, curr.pos.y+dir.y
			if nx < 0 || ny < 0 || nx >= len(grid[0]) || ny >= len(grid) {
				continue
			}
			ch := grid[ny][nx]
			if ch == '#' {
				continue
			}
			newKeys := curr.keys
			if ch >= 'a' && ch <= 'z' {
				newKeys |= 1 << (ch - 'a')
			}
			if ch >= 'A' && ch <= 'Z' && (curr.keys&(1<<(ch-'A'))) == 0 {
				continue
			}
			queue = append(queue, State{Point{nx, ny}, newKeys, curr.steps + 1})
		}
	}
	return -1
}
