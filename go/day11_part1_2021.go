package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	grid, err := readInput("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	totalFlashes := 0
	for step := 0; step < 100; step++ {
		totalFlashes += simulateStep(grid)
	}

	fmt.Println(totalFlashes)
}

func readInput(filename string) ([][]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var grid [][]int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]int, len(line))
		for i, char := range line {
			num, _ := strconv.Atoi(string(char))
			row[i] = num
		}
		grid = append(grid, row)
	}
	return grid, scanner.Err()
}

func simulateStep(grid [][]int) int {
	flashes := 0
	flashed := make(map[[2]int]bool)

	// Increase energy by 1 for all octopuses
	for y, row := range grid {
		for x := range row {
			grid[y][x]++
		}
	}

	// Flash octopuses with energy greater than 9
	for y, row := range grid {
		for x := range row {
			if grid[y][x] > 9 {
				flashes += flash(grid, x, y, flashed)
			}
		}
	}

	// Reset energy to 0 for all that flashed
	for coords := range flashed {
		grid[coords[1]][coords[0]] = 0
	}

	return flashes
}

func flash(grid [][]int, x, y int, flashed map[[2]int]bool) int {
	if flashed[[2]int{x, y}] {
		return 0
	}

	flashed[[2]int{x, y}] = true
	flashes := 1
	directions := [][2]int{{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}}

	for _, dir := range directions {
		newX, newY := x+dir[0], y+dir[1]
		if newX >= 0 && newX < len(grid[0]) && newY >= 0 && newY < len(grid) {
			grid[newY][newX]++
			if grid[newY][newX] > 9 {
				flashes += flash(grid, newX, newY, flashed)
			}
		}
	}

	return flashes
}