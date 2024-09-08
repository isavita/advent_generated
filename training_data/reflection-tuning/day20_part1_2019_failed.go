package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type Point struct {
	x, y int
}

type Portal struct {
	label string
	p1, p2 Point
}

func main() {
	maze, portals := parseMaze()
	start := findPortal(portals, "AA").p1
	end := findPortal(portals, "ZZ").p1

	steps := bfs(maze, portals, start, end)
	fmt.Println(steps)
}

func parseMaze() (map[Point]rune, []Portal) {
	maze := make(map[Point]rune)
	var portals []Portal
	scanner := bufio.NewScanner(os.Stdin)
	y := 0
	for scanner.Scan() {
		line := scanner.Text()
		for x, ch := range line {
			if ch != ' ' {
				maze[Point{x, y}] = ch
			}
		}
		y++
	}

	// Find portals
	for p, ch := range maze {
		if ch >= 'A' && ch <= 'Z' {
			label := string(ch)
			for _, dir := range []Point{{0, 1}, {1, 0}, {0, -1}, {-1, 0}} {
				next := Point{p.x + dir.x, p.y + dir.y}
				if nextCh, ok := maze[next]; ok && nextCh >= 'A' && nextCh <= 'Z' {
					label += string(nextCh)
					entrance := Point{p.x - dir.x, p.y - dir.y}
					if maze[entrance] == '.' {
						portals = append(portals, Portal{label, entrance, Point{}})
					} else {
						entrance = Point{next.x + dir.x, next.y + dir.y}
						if maze[entrance] == '.' {
							portals = append(portals, Portal{label, entrance, Point{}})
						}
					}
					break
				}
			}
		}
	}

	// Link portal pairs
	for i, p1 := range portals {
		for j, p2 := range portals {
			if i != j && p1.label == p2.label {
				portals[i].p2 = p2.p1
				portals[j].p2 = p1.p1
			}
		}
	}

	return maze, portals
}

func findPortal(portals []Portal, label string) Portal {
	for _, p := range portals {
		if p.label == label {
			return p
		}
	}
	return Portal{}
}

func bfs(maze map[Point]rune, portals []Portal, start, end Point) int {
	queue := []Point{start}
	visited := make(map[Point]bool)
	distance := make(map[Point]int)
	visited[start] = true
	distance[start] = 0

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if current == end {
			return distance[current]
		}

		// Check adjacent tiles
		for _, dir := range []Point{{0, 1}, {1, 0}, {0, -1}, {-1, 0}} {
			next := Point{current.x + dir.x, current.y + dir.y}
			if maze[next] == '.' && !visited[next] {
				queue = append(queue, next)
				visited[next] = true
				distance[next] = distance[current] + 1
			}
		}

		// Check for portal
		for _, portal := range portals {
			if portal.p1 == current && !visited[portal.p2] {
				queue = append(queue, portal.p2)
				visited[portal.p2] = true
				distance[portal.p2] = distance[current] + 1
			}
		}
	}

	return -1 // No path found
}
