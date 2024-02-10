package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type point struct {
	x, y int
}

func main() {
	file, _ := os.Open("input.txt")
	scanner := bufio.NewScanner(file)

	grid := make(map[point]int)

	for scanner.Scan() {
		line := strings.Split(scanner.Text(), " -> ")
		startCoords := strings.Split(line[0], ",")
		endCoords := strings.Split(line[1], ",")

		x1, _ := strconv.Atoi(startCoords[0])
		y1, _ := strconv.Atoi(startCoords[1])
		x2, _ := strconv.Atoi(endCoords[0])
		y2, _ := strconv.Atoi(endCoords[1])

		if x1 == x2 {
			if y1 > y2 {
				y1, y2 = y2, y1
			}
			for y := y1; y <= y2; y++ {
				grid[point{x1, y}]++
			}
		} else if y1 == y2 {
			if x1 > x2 {
				x1, x2 = x2, x1
			}
			for x := x1; x <= x2; x++ {
				grid[point{x, y1}]++
			}
		}
	}

	overlapCount := 0
	for _, v := range grid {
		if v > 1 {
			overlapCount++
		}
	}

	fmt.Println(overlapCount)
}