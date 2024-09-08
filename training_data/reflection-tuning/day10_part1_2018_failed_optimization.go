package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strings"
)

type Point struct {
	x, y, vx, vy int
}

func main() {
	points := readInput()
	message, steps := findMessage(points)
	fmt.Printf("Message appeared after %d seconds:\n", steps)
	printMessage(message)
}

func readInput() []Point {
	var points []Point
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()
		var p Point
		fmt.Sscanf(line, "position=<%d, %d> velocity=<%d, %d>", &p.x, &p.y, &p.vx, &p.vy)
		points = append(points, p)
	}
	return points
}

func findMessage(points []Point) ([]Point, int) {
	minArea := math.MaxInt64
	var minPoints []Point
	var step int

	for {
		minX, minY, maxX, maxY := getBounds(points)
		area := (maxX - minX) * (maxY - minY)

		if area < minArea {
			minArea = area
			minPoints = make([]Point, len(points))
			copy(minPoints, points)
			step++
		} else {
			// If area starts increasing, we've passed the message
			return minPoints, step - 1
		}

		// Move points
		for i := range points {
			points[i].x += points[i].vx
			points[i].y += points[i].vy
		}
	}
}

func getBounds(points []Point) (int, int, int, int) {
	minX, minY := math.MaxInt32, math.MaxInt32
	maxX, maxY := math.MinInt32, math.MinInt32

	for _, p := range points {
		minX = min(minX, p.x)
		minY = min(minY, p.y)
		maxX = max(maxX, p.x)
		maxY = max(maxY, p.y)
	}

	return minX, minY, maxX, maxY
}

func printMessage(points []Point) {
	minX, minY, maxX, maxY := getBounds(points)
	grid := make([][]byte, maxY-minY+1)
	for i := range grid {
		grid[i] = make([]byte, maxX-minX+1)
		for j := range grid[i] {
			grid[i][j] = '.'
		}
	}

	for _, p := range points {
		grid[p.y-minY][p.x-minX] = '#'
	}

	for _, row := range grid {
		fmt.Println(string(row))
	}
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
