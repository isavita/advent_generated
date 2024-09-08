package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

const gridSize = 300

func calculatePowerLevel(x, y, serialNumber int) int {
	rackID := x + 10
	powerLevel := rackID * y
	powerLevel += serialNumber
	powerLevel *= rackID
	powerLevel = (powerLevel / 100) % 10
	return powerLevel - 5
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	serialNumber, _ := strconv.Atoi(scanner.Text())

	// Calculate power levels and create prefix sum grid
	grid := make([][]int, gridSize+1)
	for i := range grid {
		grid[i] = make([]int, gridSize+1)
	}

	for y := 1; y <= gridSize; y++ {
		for x := 1; x <= gridSize; x++ {
			power := calculatePowerLevel(x, y, serialNumber)
			grid[y][x] = power + grid[y-1][x] + grid[y][x-1] - grid[y-1][x-1]
		}
	}

	// Find the 3x3 square with the largest total power
	maxPower := -1000000
	maxX, maxY := 0, 0

	for y := 1; y <= gridSize-2; y++ {
		for x := 1; x <= gridSize-2; x++ {
			power := grid[y+2][x+2] - grid[y-1][x+2] - grid[y+2][x-1] + grid[y-1][x-1]
			if power > maxPower {
				maxPower = power
				maxX, maxY = x, y
			}
		}
	}

	fmt.Printf("%d,%d\n", maxX, maxY)
}
