package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

const gridSize = 300

func main() {
	input, _ := ioutil.ReadFile("input.txt")
	serialNumber, _ := strconv.Atoi(strings.TrimSpace(string(input)))

	grid := make([][]int, gridSize+1)
	for i := range grid {
		grid[i] = make([]int, gridSize+1)
	}

	// Calculate power levels and create prefix sum grid
	for y := 1; y <= gridSize; y++ {
		for x := 1; x <= gridSize; x++ {
			power := calculatePower(x, y, serialNumber)
			grid[y][x] = power + grid[y-1][x] + grid[y][x-1] - grid[y-1][x-1]
		}
	}

	// Part 1: Find the 3x3 square with the largest total power
	maxPower := 0
	maxX, maxY := 0, 0
	for y := 3; y <= gridSize; y++ {
		for x := 3; x <= gridSize; x++ {
			power := grid[y][x] - grid[y-3][x] - grid[y][x-3] + grid[y-3][x-3]
			if power > maxPower {
				maxPower = power
				maxX, maxY = x-2, y-2
			}
		}
	}
	fmt.Printf("Part 1: %d,%d\n", maxX, maxY)

	// Part 2: Find the square of any size with the largest total power
	maxPower = 0
	maxX, maxY, maxSize := 0, 0, 0
	for size := 1; size <= gridSize; size++ {
		for y := size; y <= gridSize; y++ {
			for x := size; x <= gridSize; x++ {
				power := grid[y][x] - grid[y-size][x] - grid[y][x-size] + grid[y-size][x-size]
				if power > maxPower {
					maxPower = power
					maxX, maxY, maxSize = x-size+1, y-size+1, size
				}
			}
		}
	}
	fmt.Printf("Part 2: %d,%d,%d\n", maxX, maxY, maxSize)
}

func calculatePower(x, y, serialNumber int) int {
	rackID := x + 10
	power := rackID * y
	power += serialNumber
	power *= rackID
	power = (power / 100) % 10
	return power - 5
}
