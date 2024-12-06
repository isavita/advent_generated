package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type state struct {
	x, y, dir int
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	var grid [][]rune
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		row := []rune(line)
		grid = append(grid, row)
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	h := len(grid)
	w := len(grid[0])

	var startX, startY, startDir int
	for i := 0; i < h; i++ {
		for j := 0; j < w; j++ {
			switch grid[i][j] {
			case '^':
				startX, startY, startDir = j, i, 0
			case '>':
				startX, startY, startDir = j, i, 1
			case 'v':
				startX, startY, startDir = j, i, 2
			case '<':
				startX, startY, startDir = j, i, 3
			}
		}
	}
	// Convert the guard's starting cell to '.' for simulation
	grid[startY][startX] = '.'

	canLoop := 0
	for y := 0; y < h; y++ {
		for x := 0; x < w; x++ {
			if x == startX && y == startY {
				continue
			}
			if grid[y][x] != '.' {
				continue
			}
			grid[y][x] = '#'
			if loops(grid, startX, startY, startDir) {
				canLoop++
			}
			grid[y][x] = '.'
		}
	}

	fmt.Println(canLoop)
}

func loops(grid [][]rune, sx, sy, sdir int) bool {
	h := len(grid)
	w := len(grid[0])
	dirs := [][2]int{{0, -1}, {1, 0}, {0, 1}, {-1, 0}}
	x, y, dir := sx, sy, sdir
	seen := map[state]bool{}
	for step := 0; step < 2000000; step++ {
		st := state{x, y, dir}
		if seen[st] {
			return true
		}
		seen[st] = true
		dx, dy := dirs[dir][0], dirs[dir][1]
		nx, ny := x+dx, y+dy
		if nx < 0 || nx >= w || ny < 0 || ny >= h {
			return false
		}
		if grid[ny][nx] == '#' {
			dir = (dir + 1) % 4
			continue
		}
		x, y = nx, ny
	}
	return false
}
