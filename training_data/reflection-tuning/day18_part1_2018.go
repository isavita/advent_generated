package main

import (
	"bufio"
	"fmt"
	"os"
)

const (
	Open       = '.'
	Trees      = '|'
	Lumberyard = '#'
)

func main() {
	grid := readInput("input.txt")
	rows, cols := len(grid), len(grid[0])
	
	for i := 0; i < 10; i++ {
		newGrid := make([][]byte, rows)
		for j := range newGrid {
			newGrid[j] = make([]byte, cols)
		}
		
		for r := 0; r < rows; r++ {
			for c := 0; c < cols; c++ {
				adjacent := countAdjacent(grid, r, c)
				switch grid[r][c] {
				case Open:
					if adjacent[Trees] >= 3 {
						newGrid[r][c] = Trees
					} else {
						newGrid[r][c] = Open
					}
				case Trees:
					if adjacent[Lumberyard] >= 3 {
						newGrid[r][c] = Lumberyard
					} else {
						newGrid[r][c] = Trees
					}
				case Lumberyard:
					if adjacent[Lumberyard] >= 1 && adjacent[Trees] >= 1 {
						newGrid[r][c] = Lumberyard
					} else {
						newGrid[r][c] = Open
					}
				}
			}
		}
		grid = newGrid
	}
	
	wooded, lumberyards := 0, 0
	for _, row := range grid {
		for _, acre := range row {
			if acre == Trees {
				wooded++
			} else if acre == Lumberyard {
				lumberyards++
			}
		}
	}
	
	fmt.Println(wooded * lumberyards)
}

func readInput(filename string) [][]byte {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var grid [][]byte
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		grid = append(grid, []byte(scanner.Text()))
	}
	return grid
}

func countAdjacent(grid [][]byte, r, c int) map[byte]int {
	count := make(map[byte]int)
	rows, cols := len(grid), len(grid[0])
	for dr := -1; dr <= 1; dr++ {
		for dc := -1; dc <= 1; dc++ {
			if dr == 0 && dc == 0 {
				continue
			}
			nr, nc := r+dr, c+dc
			if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
				count[grid[nr][nc]]++
			}
		}
	}
	return count
}
