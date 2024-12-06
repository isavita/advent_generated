package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

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
	var x, y int
	var dirX, dirY int
	dirs := [][2]int{{0, -1}, {1, 0}, {0, 1}, {-1, 0}}
	var dirIdx int
	found := false
	for i := 0; i < h && !found; i++ {
		for j := 0; j < w && !found; j++ {
			switch grid[i][j] {
			case '^':
				x, y = j, i
				dirIdx = 0
				dirX, dirY = dirs[dirIdx][0], dirs[dirIdx][1]
				found = true
			case '>':
				x, y = j, i
				dirIdx = 1
				dirX, dirY = dirs[dirIdx][0], dirs[dirIdx][1]
				found = true
			case 'v':
				x, y = j, i
				dirIdx = 2
				dirX, dirY = dirs[dirIdx][0], dirs[dirIdx][1]
				found = true
			case '<':
				x, y = j, i
				dirIdx = 3
				dirX, dirY = dirs[dirIdx][0], dirs[dirIdx][1]
				found = true
			}
		}
	}

	visited := map[[2]int]bool{}
	visited[[2]int{x, y}] = true

	for {
		nx, ny := x+dirX, y+dirY
		if nx < 0 || nx >= w || ny < 0 || ny >= h {
			break
		}
		if grid[ny][nx] == '#' {
			dirIdx = (dirIdx + 1) % 4
			dirX, dirY = dirs[dirIdx][0], dirs[dirIdx][1]
			continue
		}
		x, y = nx, ny
		visited[[2]int{x, y}] = true
	}

	fmt.Println(len(visited))
}
