package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type RebootStep struct {
	action string
	xStart int
	xEnd   int
	yStart int
	yEnd   int
	zStart int
	zEnd   int
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	rebootSteps := []RebootStep{}

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		step := parseRebootStep(line)
		rebootSteps = append(rebootSteps, step)
	}

	minCoord, maxCoord := -50, 50
	cubeGrid := createCubeGrid(minCoord, maxCoord)
	executeRebootSteps(cubeGrid, rebootSteps)
	onCubes := countOnCubes(cubeGrid)

	fmt.Println(onCubes)
}

func parseRebootStep(line string) RebootStep {
	parts := strings.Split(line, " ")

	action := parts[0]
	parts = strings.Split(parts[1], ",")
	xRange := strings.Split(parts[0][2:], "..")
	yRange := strings.Split(parts[1][2:], "..")
	zRange := strings.Split(parts[2][2:], "..")

	xStart, _ := strconv.Atoi(xRange[0])
	xEnd, _ := strconv.Atoi(xRange[1])
	yStart, _ := strconv.Atoi(yRange[0])
	yEnd, _ := strconv.Atoi(yRange[1])
	zStart, _ := strconv.Atoi(zRange[0])
	zEnd, _ := strconv.Atoi(zRange[1])

	return RebootStep{action, xStart, xEnd, yStart, yEnd, zStart, zEnd}
}

func createCubeGrid(minCoord, maxCoord int) [][][]bool {
	gridSize := maxCoord - minCoord + 1
	grid := make([][][]bool, gridSize)

	for i := range grid {
		grid[i] = make([][]bool, gridSize)
		for j := range grid[i] {
			grid[i][j] = make([]bool, gridSize)
		}
	}

	return grid
}

func executeRebootSteps(cubeGrid [][][]bool, rebootSteps []RebootStep) {
	for _, step := range rebootSteps {
		if !(step.xStart >= -50 && step.xEnd <= 50 && step.yStart >= -50 && step.yEnd <= 50 && step.zStart >= -50 && step.zEnd <= 50) {
			continue
		}
		for x := step.xStart; x <= step.xEnd; x++ {
			for y := step.yStart; y <= step.yEnd; y++ {
				for z := step.zStart; z <= step.zEnd; z++ {
					cubeGrid[x+50][y+50][z+50] = step.action == "on"
				}
			}
		}
	}
}

func countOnCubes(cubeGrid [][][]bool) int {
	count := 0

	for i := range cubeGrid {
		for j := range cubeGrid[i] {
			for k := range cubeGrid[i][j] {
				if cubeGrid[i][j][k] {
					count++
				}
			}
		}
	}

	return count
}