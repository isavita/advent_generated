package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}

	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	grid := make([][]byte, len(lines))

	for i, line := range lines {
		grid[i] = []byte(line)
	}
	fmt.Println(findSafeStep(grid))
}

func findSafeStep(grid [][]byte) int {
	step := 0
	for {

		eastMoved := moveEast(grid)
		southMoved := moveSouth(grid)
		step++

		if !eastMoved && !southMoved {
			break
		}
	}
	return step
}

func moveEast(grid [][]byte) bool {
	moved := false
	height := len(grid)
	width := len(grid[0])

	oldPositions := make([][]byte, height)
	for i := 0; i < height; i++ {
		oldPositions[i] = make([]byte, width)
	}
	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			if grid[y][x] == '>' {
				nextX := (x + 1) % width
				if grid[y][nextX] == '.' {
					oldPositions[y][x] = '.'
					grid[y][nextX] = '>'
					x++
					moved = true
				}
			}
		}
	}
	freeEmptyPositions(grid, oldPositions)

	return moved
}

func moveSouth(grid [][]byte) bool {
	moved := false
	height := len(grid)
	width := len(grid[0])

	oldPositions := make([][]byte, height)
	for i := 0; i < height; i++ {
		oldPositions[i] = make([]byte, width)
	}
	for x := 0; x < width; x++ {
		for y := 0; y < height; y++ {
			if grid[y][x] == 'v' {
				nextY := (y + 1) % height
				if grid[nextY][x] == '.' {
					oldPositions[y][x] = '.'
					grid[nextY][x] = 'v'
					y++
					moved = true
				}
			}
		}
	}
	freeEmptyPositions(grid, oldPositions)

	return moved
}

func freeEmptyPositions(grid [][]byte, oldPostion [][]byte) {
	for y := 0; y < len(grid); y++ {
		for x := 0; x < len(grid[0]); x++ {
			if oldPostion[y][x] == '.' {
				grid[y][x] = '.'
			}
		}
	}
}