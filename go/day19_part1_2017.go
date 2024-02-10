package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var grid [][]rune
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		grid = append(grid, []rune(scanner.Text()))
	}

	x, y := 0, 0
	for i := range grid[0] {
		if grid[0][i] == '|' {
			x = i
			break
		}
	}

	dx, dy := 0, 1
	var letters []rune

	for {
		if x < 0 || x >= len(grid[0]) || y < 0 || y >= len(grid) {
			break
		}

		cell := grid[y][x]

		if cell == ' ' {
			break
		}

		if cell >= 'A' && cell <= 'Z' {
			letters = append(letters, cell)
		}

		if cell == '+' {
			if dx == 0 {
				if x > 0 && (grid[y][x-1] == '-' || (grid[y][x-1] >= 'A' && grid[y][x-1] <= 'Z')) {
					dx, dy = -1, 0
				} else {
					dx, dy = 1, 0
				}
			} else {
				if y > 0 && (grid[y-1][x] == '|' || (grid[y-1][x] >= 'A' && grid[y-1][x] <= 'Z')) {
					dx, dy = 0, -1
				} else {
					dx, dy = 0, 1
				}
			}
		}

		x += dx
		y += dy
	}

	fmt.Println(string(letters))
}