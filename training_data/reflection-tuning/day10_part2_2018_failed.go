package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"regexp"
	"strconv"
)

type Point struct {
	x, y, vx, vy int
}

func main() {
	points := readInput("input.txt")
	seconds, minX, maxX, minY, maxY := findMessage(points)

	// Print the message
	grid := make([][]byte, maxY-minY+1)
	for i := range grid {
		grid[i] = make([]byte, maxX-minX+1)
		for j := range grid[i] {
			grid[i][j] = '.'
		}
	}

	for _, p := range points {
		x, y := p.x-minX, p.y-minY
		grid[y][x] = '#'
	}

	fmt.Println("Message:")
	for _, row := range grid {
		fmt.Println(string(row))
	}

	fmt.Printf("Seconds: %d\n", seconds)
}

func readInput(filename string) []Point {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var points []Point
	re := regexp.MustCompile(`position=<\s*(-?\d+),\s*(-?\d+)>\s*velocity=<\s*(-?\d+),\s*(-?\d+)>`)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		matches := re.FindStringSubmatch(scanner.Text())
		if matches != nil {
			x, _ := strconv.Atoi(matches[1])
			y, _ := strconv.Atoi(matches[2])
			vx, _ := strconv.Atoi(matches[3])
			vy, _ := strconv.Atoi(matches[4])
			points = append(points, Point{x, y, vx, vy})
		}
	}
	return points
}

func findMessage(points []Point) (int, int, int, int, int) {
	seconds := 0
	minArea := math.MaxInt64
	var resultMinX, resultMaxX, resultMinY, resultMaxY int

	for {
		minX, maxX, minY, maxY := getBoundingBox(points)
		area := (maxX - minX) * (maxY - minY)

		if area < minArea {
			minArea = area
			resultMinX, resultMaxX, resultMinY, resultMaxY = minX, maxX, minY, maxY
		} else if area > minArea {
			// If area starts increasing, we've passed the message formation
			return seconds - 1, resultMinX, resultMaxX, resultMinY, resultMaxY
		}

		// Move points
		for i := range points {
			points[i].x += points[i].vx
			points[i].y += points[i].vy
		}
		seconds++
	}
}

func getBoundingBox(points []Point) (int, int, int, int) {
	minX, minY := math.MaxInt32, math.MaxInt32
	maxX, maxY := math.MinInt32, math.MinInt32

	for _, p := range points {
		minX = min(minX, p.x)
		maxX = max(maxX, p.x)
		minY = min(minY, p.y)
		maxY = max(maxY, p.y)
	}

	return minX, maxX, minY, maxY
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
