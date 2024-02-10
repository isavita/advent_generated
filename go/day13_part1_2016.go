package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

func isWall(favoriteNumber, x, y int) bool {
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

func bfs(start, target Point, favoriteNumber int) int {
	visited := make(map[Point]bool)
	queue := []Point{start}
	steps := 0

	for len(queue) > 0 {
		size := len(queue)
		for i := 0; i < size; i++ {
			point := queue[i]
			if point == target {
				return steps
			}

			for _, delta := range []Point{{1, 0}, {-1, 0}, {0, 1}, {0, -1}} {
				next := Point{point.x + delta.x, point.y + delta.y}
				if next.x >= 0 && next.y >= 0 && !isWall(favoriteNumber, next.x, next.y) && !visited[next] {
					visited[next] = true
					queue = append(queue, next)
				}
			}
		}
		queue = queue[size:]
		steps++
	}

	return -1
}

func main() {
	n, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	num := strings.TrimSpace(string(n))
	favoriteNumber, _ := strconv.Atoi(num)
	start := Point{1, 1}
	target := Point{31, 39}
	// Target coordinate
	steps := bfs(start, target, favoriteNumber)
	fmt.Println(steps)
}