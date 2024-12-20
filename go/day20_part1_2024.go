package main

import (
	"bufio"
	"fmt"
	"os"
)

type point struct{ x, y int }

func main() {
	f, _ := os.Open("input.txt")
	defer f.Close()
	scanner := bufio.NewScanner(f)
	var grid []string
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}

	h, w := len(grid), len(grid[0])
	var S, E point
	var trackCells []point
	walls := make([][]bool, h)
	for i := range walls {
		walls[i] = make([]bool, w)
	}

	for i := 0; i < h; i++ {
		for j := 0; j < w; j++ {
			ch := grid[i][j]
			if ch == 'S' {
				S = point{i, j}
			} else if ch == 'E' {
				E = point{i, j}
			}
			if ch == '#' {
				walls[i][j] = true
			} else {
				trackCells = append(trackCells, point{i, j})
			}
		}
	}

	// Directions
	dirs := []point{{1, 0}, {-1, 0}, {0, 1}, {0, -1}}

	// BFS function that returns dist from start to all reachable points
	// ignoring walls if ignoreWalls==false (normal) or ignoring them still returns wall as unreachable
	// This BFS only runs in normal mode (no cheat) so walls are blocked.
	normalDistFrom := func(start point) [][]int {
		dist := make([][]int, h)
		for i := range dist {
			dist[i] = make([]int, w)
			for j := range dist[i] {
				dist[i][j] = -1
			}
		}
		dist[start.x][start.y] = 0
		q := []point{start}
		for len(q) > 0 {
			cur := q[0]
			q = q[1:]
			for _, d := range dirs {
				nx, ny := cur.x+d.x, cur.y+d.y
				if nx < 0 || nx >= h || ny < 0 || ny >= w {
					continue
				}
				if walls[nx][ny] {
					continue
				}
				if dist[nx][ny] == -1 {
					dist[nx][ny] = dist[cur.x][cur.y] + 1
					q = append(q, point{nx, ny})
				}
			}
		}
		return dist
	}

	distFromS := normalDistFrom(S)
	distFromE := normalDistFrom(E)

	// If no normal path from S to E
	if distFromS[E.x][E.y] == -1 {
		fmt.Println(0)
		return
	}

	normalCost := distFromS[E.x][E.y]

	// We need to consider all possible cheats.
	// A cheat: choose a startCheatPos on track, then move through up to 2 steps ignoring walls (but must end on track).
	// Then continue to E normally.
	// The cost with cheat = dist(S->startCheatPos) + 2 (for the cheat steps) + dist(endCheatPos->E).
	// We must find all cheats that actually improve cost: saving = normalCost - (that cost).
	// Count how many have saving >= 100.

	// Precompute which cells are track (not wall).
	isTrack := func(x, y int) bool {
		if x < 0 || x >= h || y < 0 || y >= w {
			return false
		}
		return !walls[x][y]
	}

	// For each track cell as startCheatPos:
	// Try all ways to move up to 2 steps ignoring walls:
	// We do a small BFS/DFS with up to 2 steps:
	possibleCheats := 0

	for _, startPos := range trackCells {
		sd := distFromS[startPos.x][startPos.y]
		if sd == -1 {
			continue
		}
		// We'll find all reachable end positions from startPos with exactly 2 steps ignoring walls.
		// The cheat must end on a track cell.
		// Let's just brute force:
		for _, d1 := range dirs {
			m1 := point{startPos.x + d1.x, startPos.y + d1.y}
			// Step 1 can go through wall or track, must be within grid
			if m1.x < 0 || m1.x >= h || m1.y < 0 || m1.y >= w {
				continue
			}
			for _, d2 := range dirs {
				m2 := point{m1.x + d2.x, m1.y + d2.y}
				if m2.x < 0 || m2.x >= h || m2.y < 0 || m2.y >= w {
					continue
				}
				// After 2 steps m2 must be track
				if !isTrack(m2.x, m2.y) {
					continue
				}
				ed := distFromE[m2.x][m2.y]
				if ed == -1 {
					continue
				}
				newCost := sd + 2 + ed
				saving := normalCost - newCost
				if saving >= 100 {
					possibleCheats++
				}
			}
		}
	}

	fmt.Println(possibleCheats)
}
