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

func isWall(x, y, favoriteNumber int) bool {
	sum := x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
	bits := 0
	for sum > 0 {
		bits += sum & 1
		sum >>= 1
	}
	return bits%2 != 0
}

func bfs(favoriteNumber, maxSteps int) (int, int) {
	start := Point{1, 1}
	target := Point{31, 39}
	queue := []Point{start}
	visited := map[Point]int{start: 0}
	directions := []Point{{0, 1}, {1, 0}, {0, -1}, {-1, 0}}

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if current == target {
			return visited[current], len(visited)
		}

		for _, dir := range directions {
			next := Point{current.x + dir.x, current.y + dir.y}
			if next.x < 0 || next.y < 0 || isWall(next.x, next.y, favoriteNumber) {
				continue
			}
			if _, seen := visited[next]; !seen && visited[current] < maxSteps {
				visited[next] = visited[current] + 1
				queue = append(queue, next)
			}
		}
	}

	return -1, len(visited)
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Scan()
	favoriteNumber, _ := strconv.Atoi(scanner.Text())

	steps, locations := bfs(favoriteNumber, 50)
	fmt.Println("Part 1:", steps)
	fmt.Println("Part 2:", locations)
}
