package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// Point represents a location on the grid
type Point struct {
	x, y int
}

func main() {
	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var points []Point
	scanner := bufio.NewScanner(file)

	// Parse the input
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		parts := strings.Split(line, ",")
		if len(parts) != 2 {
			// Skip malformed lines or handle error
			continue
		}

		x, err1 := strconv.Atoi(parts[0])
		y, err2 := strconv.Atoi(parts[1])

		if err1 != nil || err2 != nil {
			log.Fatalf("Invalid coordinate format: %s", line)
		}

		points = append(points, Point{x, y})
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	// Calculate max area
	// Area is (abs(x1-x2) + 1) * (abs(y1-y2) + 1)
	maxArea := int64(0)

	// We iterate all pairs (i, j). We can start j from i to include the case
	// where a single point forms a 1x1 rectangle, though that won't be the max.
	for i := 0; i < len(points); i++ {
		for j := i; j < len(points); j++ {
			p1 := points[i]
			p2 := points[j]

			// Calculate width and height inclusive
			dx := int64(abs(p1.x - p2.x)) + 1
			dy := int64(abs(p1.y - p2.y)) + 1
			
			area := dx * dy

			if area > maxArea {
				maxArea = area
			}
		}
	}

	fmt.Printf("Largest area: %d\n", maxArea)
}

// abs returns the absolute value of an integer
func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}
