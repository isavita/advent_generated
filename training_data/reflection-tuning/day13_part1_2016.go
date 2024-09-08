package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type Point struct {
	x, y int
}

type Queue []Point

func (q *Queue) Push(p Point) {
	*q = append(*q, p)
}

func (q *Queue) Pop() Point {
	p := (*q)[0]
	*q = (*q)[1:]
	return p
}

func (q *Queue) Empty() bool {
	return len(*q) == 0
}

func isWall(x, y, favoriteNumber int) bool {
	value := x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
	bits := 0
	for value > 0 {
		bits += value & 1
		value >>= 1
	}
	return bits%2 != 0
}

func bfs(start, target Point, favoriteNumber int) int {
	queue := Queue{start}
	visited := make(map[Point]bool)
	distance := make(map[Point]int)
	visited[start] = true
	distance[start] = 0

	for !queue.Empty() {
		current := queue.Pop()

		if current == target {
			return distance[current]
		}

		for _, dir := range []Point{{0, 1}, {1, 0}, {0, -1}, {-1, 0}} {
			next := Point{current.x + dir.x, current.y + dir.y}
			if next.x < 0 || next.y < 0 || visited[next] || isWall(next.x, next.y, favoriteNumber) {
				continue
			}
			queue.Push(next)
			visited[next] = true
			distance[next] = distance[current] + 1
		}
	}

	return -1 // Target not reachable
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	favoriteNumber, _ := strconv.Atoi(scanner.Text())

	start := Point{1, 1}
	target := Point{31, 39}

	steps := bfs(start, target, favoriteNumber)
	fmt.Println(steps)
}
