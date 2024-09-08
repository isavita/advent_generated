package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

const (
	Open       = '.'
	Trees      = '|'
	Lumberyard = '#'
)

type Grid [][]byte

func main() {
	grid := readInput("input.txt")
	
	// Part 1
	for i := 0; i < 10; i++ {
		grid = simulate(grid)
	}
	fmt.Println("Part 1:", resourceValue(grid))

	// Part 2
	grid = readInput("input.txt") // Reset grid
	target := 1000000000
	seen := make(map[string]int)
	minute := 0

	for minute < target {
		key := gridToString(grid)
		if prev, ok := seen[key]; ok {
			cycleLength := minute - prev
			remaining := target - minute
			minute += (remaining / cycleLength) * cycleLength
			break
		}
		seen[key] = minute
		grid = simulate(grid)
		minute++
	}

	for minute < target {
		grid = simulate(grid)
		minute++
	}

	fmt.Println("Part 2:", resourceValue(grid))
}

func readInput(filename string) Grid {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)
	var grid Grid
	for scanner.Scan() {
		grid = append(grid, []byte(scanner.Text()))
	}
	return grid
}

func simulate(grid Grid) Grid {
	newGrid := make(Grid, len(grid))
	for i := range newGrid {
		newGrid[i] = make([]byte, len(grid[i]))
	}

	for i := range grid {
		for j := range grid[i] {
			trees, lumberyards := countAdjacent(grid, i, j)
			switch grid[i][j] {
			case Open:
				if trees >= 3 {
					newGrid[i][j] = Trees
				} else {
					newGrid[i][j] = Open
				}
			case Trees:
				if lumberyards >= 3 {
					newGrid[i][j] = Lumberyard
				} else {
					newGrid[i][j] = Trees
				}
			case Lumberyard:
				if lumberyards >= 1 && trees >= 1 {
					newGrid[i][j] = Lumberyard
				} else {
					newGrid[i][j] = Open
				}
			}
		}
	}
	return newGrid
}

func countAdjacent(grid Grid, x, y int) (int, int) {
	trees, lumberyards := 0, 0
	for dx := -1; dx <= 1; dx++ {
		for dy := -1; dy <= 1; dy++ {
			if dx == 0 && dy == 0 {
				continue
			}
			nx, ny := x+dx, y+dy
			if nx >= 0 && nx < len(grid) && ny >= 0 && ny < len(grid[0]) {
				switch grid[nx][ny] {
				case Trees:
					trees++
				case Lumberyard:
					lumberyards++
				}
			}
		}
	}
	return trees, lumberyards
}

func resourceValue(grid Grid) int {
	trees, lumberyards := 0, 0
	for _, row := range grid {
		for _, acre := range row {
			if acre == Trees {
				trees++
			} else if acre == Lumberyard {
				lumberyards++
			}
		}
	}
	return trees * lumberyards
}

func gridToString(grid Grid) string {
	var sb strings.Builder
	for _, row := range grid {
		sb.Write(row)
	}
	return sb.String()
}
