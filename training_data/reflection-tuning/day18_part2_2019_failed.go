package main

import (
	"fmt"
	"math"
	"strings"
)

type Point struct {
	x, y int
}

type State struct {
	pos   [4]Point
	keys  uint32
	steps int
}

func shortestPath(grid []string, part2 bool) int {
	height, width := len(grid), len(grid[0])
	keys := make(map[byte]Point)
	starts := make([]Point, 0, 4)

	// Find keys and starting positions
	for y, row := range grid {
		for x, ch := range row {
			if ch >= 'a' && ch <= 'z' {
				keys[byte(ch)] = Point{x, y}
			} else if ch == '@' {
				starts = append(starts, Point{x, y})
			}
		}
	}

	if part2 {
		// Modify the grid for part 2
		cx, cy := starts[0].x, starts[0].y
		grid[cy-1] = grid[cy-1][:cx-1] + "@#@" + grid[cy-1][cx+2:]
		grid[cy] = grid[cy][:cx-1] + "###" + grid[cy][cx+2:]
		grid[cy+1] = grid[cy+1][:cx-1] + "@#@" + grid[cy+1][cx+2:]
		starts = []Point{{cx - 1, cy - 1}, {cx + 1, cy - 1}, {cx - 1, cy + 1}, {cx + 1, cy + 1}}
	}

	// Precompute distances between keys and entrances
	distances := precomputeDistances(grid, keys, starts)

	// BFS
	queue := []State{{starts, 0, 0}}
	visited := make(map[[5]int]bool)

	for len(queue) > 0 {
		state := queue[0]
		queue = queue[1:]

		if state.keys == (1<<len(keys))-1 {
			return state.steps
		}

		key := [5]int{state.pos[0].x, state.pos[0].y, state.pos[1].x, state.pos[1].y, int(state.keys)}
		if visited[key] {
			continue
		}
		visited[key] = true

		for i, pos := range state.pos {
			for k, keyPos := range keys {
				if state.keys&(1<<(k-'a')) != 0 {
					continue
				}
				dist, doors := distances[pos][keyPos]
				if dist == math.MaxInt32 || (doors & ^state.keys) != 0 {
					continue
				}
				newPos := state.pos
				newPos[i] = keyPos
				newState := State{newPos, state.keys | (1 << (k - 'a')), state.steps + dist}
				queue = append(queue, newState)
			}
		}
	}

	return -1 // No solution found
}

func precomputeDistances(grid []string, keys map[byte]Point, starts []Point) map[Point]map[Point][2]int {
	allPoints := make([]Point, 0, len(keys)+len(starts))
	for _, p := range starts {
		allPoints = append(allPoints, p)
	}
	for _, p := range keys {
		allPoints = append(allPoints, p)
	}

	distances := make(map[Point]map[Point][2]int)
	for _, from := range allPoints {
		distances[from] = make(map[Point][2]int)
		for _, to := range allPoints {
			if from == to {
				continue
			}
			dist, doors := bfs(grid, from, to)
			distances[from][to] = [2]int{dist, doors}
		}
	}
	return distances
}

func bfs(grid []string, start, end Point) (int, int) {
	queue := []Point{start}
	visited := make(map[Point]bool)
	dist := make(map[Point]int)
	doors := make(map[Point]int)

	for len(queue) > 0 {
		curr := queue[0]
		queue = queue[1:]

		if curr == end {
			return dist[curr], doors[curr]
		}

		for _, dir := range [][2]int{{0, 1}, {1, 0}, {0, -1}, {-1, 0}} {
			next := Point{curr.x + dir[0], curr.y + dir[1]}
			if next.x < 0 || next.y < 0 || next.x >= len(grid[0]) || next.y >= len(grid) {
				continue
			}
			ch := grid[next.y][next.x]
			if ch == '#' || visited[next] {
				continue
			}
			visited[next] = true
			dist[next] = dist[curr] + 1
			doors[next] = doors[curr]
			if ch >= 'A' && ch <= 'Z' {
				doors[next] |= 1 << (ch - 'A')
			}
			queue = append(queue, next)
		}
	}

	return math.MaxInt32, 0
}

func main() {
	input := `#############
#DcBa.#.GhKl#
#.###@#@#I###
#e#d#####j#k#
###C#@#@###J#
#fEbA.#.FgHi#
#############`

	grid := strings.Split(input, "\n")

	fmt.Println("Part 1:", shortestPath(grid, false))
	fmt.Println("Part 2:", shortestPath(grid, true))
}
