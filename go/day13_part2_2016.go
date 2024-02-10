package main

import (
	"fmt"
)

const favoriteNumber = 1362 // Replace with your puzzle input

type Point struct {
	x, y int
}

func isWall(x, y int) bool {
	num := x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
	bits := 0
	for num > 0 {
		if num%2 == 1 {
			bits++
		}
		num /= 2
	}
	return bits%2 != 0
}

func bfsMaxSteps(start Point, maxSteps int) int {
	visited := make(map[Point]bool)
	queue := []Point{start}
	visited[start] = true
	steps := 0

	for len(queue) > 0 && steps < maxSteps {
		size := len(queue)
		for i := 0; i < size; i++ {
			point := queue[i]

			for _, delta := range []Point{{1, 0}, {-1, 0}, {0, 1}, {0, -1}} {
				next := Point{point.x + delta.x, point.y + delta.y}
				if next.x >= 0 && next.y >= 0 && !isWall(next.x, next.y) && !visited[next] {
					visited[next] = true
					queue = append(queue, next)
				}
			}
		}
		queue = queue[size:]
		steps++
	}

	return len(visited)
}

func main() {
	start := Point{1, 1}
	reachableLocations := bfsMaxSteps(start, 50)
	fmt.Println(reachableLocations)
}