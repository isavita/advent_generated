package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

const gridSize = 1000

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	grid := make([][]int, gridSize)
	for i := range grid {
		grid[i] = make([]int, gridSize)
	}

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		instruction := scanner.Text()
		processInstruction(instruction, grid)
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	fmt.Println(totalBrightness(grid))
}

func processInstruction(instruction string, grid [][]int) {
	parts := strings.Fields(instruction)
	var startX, startY, endX, endY int
	fmt.Sscanf(parts[len(parts)-3], "%d,%d", &startX, &startY)
	fmt.Sscanf(parts[len(parts)-1], "%d,%d", &endX, &endY)

	for x := startX; x <= endX; x++ {
		for y := startY; y <= endY; y++ {
			switch {
			case strings.HasPrefix(instruction, "turn on"):
				grid[x][y]++
			case strings.HasPrefix(instruction, "turn off"):
				if grid[x][y] > 0 {
					grid[x][y]--
				}
			case strings.HasPrefix(instruction, "toggle"):
				grid[x][y] += 2
			}
		}
	}
}

func totalBrightness(grid [][]int) int {
	brightness := 0
	for _, row := range grid {
		for _, light := range row {
			brightness += light
		}
	}
	return brightness
}