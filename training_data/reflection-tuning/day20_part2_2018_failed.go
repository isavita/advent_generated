package main

import (
	"fmt"
	"strings"
)

type Point struct {
	x, y int
}

func main() {
	input := "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
	grid := make(map[Point]bool)
	current := Point{0, 0}
	stack := []Point{}
	maxDist := 0
	rooms1000Doors := 0

	grid[current] = true

	for i := 1; i < len(input)-1; i++ {
		switch input[i] {
		case 'N':
			current.y++
		case 'S':
			current.y--
		case 'E':
			current.x++
		case 'W':
			current.x--
		case '(':
			stack = append(stack, current)
		case ')':
			current = stack[len(stack)-1]
			stack = stack[:len(stack)-1]
		case '|':
			current = stack[len(stack)-1]
		}

		if input[i] != '(' && input[i] != ')' && input[i] != '|' {
			grid[current] = true
		}
	}

	distances := bfs(grid, Point{0, 0})

	for _, dist := range distances {
		if dist > maxDist {
			maxDist = dist
		}
		if dist >= 1000 {
			rooms1000Doors++
		}
	}

	fmt.Printf("Part 1: %d\n", maxDist)
	fmt.Printf("Part 2: %d\n", rooms1000Doors)
}

func bfs(grid map[Point]bool, start Point) map[Point]int {
	queue := []Point{start}
	distances := map[Point]int{start: 0}
	dirs := []Point{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		for _, dir := range dirs {
			next := Point{current.x + dir.x, current.y + dir.y}
			if grid[next] && distances[next] == 0 {
				distances[next] = distances[current] + 1
				queue = append(queue, next)
			}
		}
	}

	return distances
}
