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
	level int
}

func main() {
	maze, portals, start, end := parseMaze("input.txt")
	steps := bfs(maze, portals, start, end)
	fmt.Println(steps)
}

func parseMaze(filename string) ([][]rune, map[string][]Point, Point, Point) {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)

	var maze [][]rune
	for scanner.Scan() {
		maze = append(maze, []rune(scanner.Text()))
	}

	portals := make(map[string][]Point)
	var start, end Point

	for y := 0; y < len(maze); y++ {
		for x := 0; x < len(maze[y]); x++ {
			if maze[y][x] >= 'A' && maze[y][x] <= 'Z' {
				name := string(maze[y][x])
				if x+1 < len(maze[y]) && maze[y][x+1] >= 'A' && maze[y][x+1] <= 'Z' {
					name += string(maze[y][x+1])
					pos := Point{x: x + 2, y: y}
					if x+2 >= len(maze[y]) || maze[y][x+2] != '.' {
						pos = Point{x: x - 1, y: y}
					}
					portals[name] = append(portals[name], pos)
				} else if y+1 < len(maze) && maze[y+1][x] >= 'A' && maze[y+1][x] <= 'Z' {
					name += string(maze[y+1][x])
					pos := Point{x: x, y: y + 2}
					if y+2 >= len(maze) || maze[y+2][x] != '.' {
						pos = Point{x: x, y: y - 1}
					}
					portals[name] = append(portals[name], pos)
				}
			}
		}
	}

	start, end = portals["AA"][0], portals["ZZ"][0]
	delete(portals, "AA")
	delete(portals, "ZZ")

	return maze, portals, start, end
}

func bfs(maze [][]rune, portals map[string][]Point, start, end Point) int {
	queue := []State{{pos: start, level: 0}}
	visited := make(map[State]bool)
	steps := 0

	for len(queue) > 0 {
		size := len(queue)
		for i := 0; i < size; i++ {
			curr := queue[0]
			queue = queue[1:]

			if curr.pos == end && curr.level == 0 {
				return steps
			}

			if visited[curr] {
				continue
			}
			visited[curr] = true

			for _, dir := range []Point{{0, 1}, {1, 0}, {0, -1}, {-1, 0}} {
				next := Point{curr.pos.x + dir.x, curr.pos.y + dir.y}
				if next.x < 0 || next.y < 0 || next.x >= len(maze[0]) || next.y >= len(maze) {
					continue
				}
				if maze[next.y][next.x] == '.' {
					queue = append(queue, State{pos: next, level: curr.level})
				}
			}

			for _, portalPoints := range portals {
				for i, p := range portalPoints {
					if p == curr.pos {
						isOuter := p.x == 2 || p.y == 2 || p.x == len(maze[0])-3 || p.y == len(maze)-3
						if (isOuter && curr.level > 0) || (!isOuter && curr.level >= 0) {
							nextLevel := curr.level
							if isOuter {
								nextLevel--
							} else {
								nextLevel++
							}
							nextPos := portalPoints[1-i]
							queue = append(queue, State{pos: nextPos, level: nextLevel})
						}
					}
				}
			}
		}
		steps++
	}

	return -1 // No path found
}
