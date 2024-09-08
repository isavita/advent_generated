package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

type Point struct {
	x, y, vx, vy int
}

func main() {
	points := readInput("input.txt")
	
	minArea := int(1e9)
	minTime := 0
	
	for t := 0; t < 100000; t++ { // Arbitrary large number
		movePoints(points)
		minX, maxX, minY, maxY := getBounds(points)
		area := (maxX - minX) * (maxY - minY)
		
		if area < minArea {
			minArea = area
			minTime = t + 1
		} else if area > minArea {
			// If area starts increasing, we've passed the message
			break
		}
	}
	
	// Move points to the time of smallest area
	for i := 0; i < minTime; i++ {
		movePoints(points)
	}
	
	// Print the message
	printMessage(points)
	fmt.Printf("Message appeared after %d seconds\n", minTime)
}

func readInput(filename string) []Point {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)
	
	var points []Point
	re := regexp.MustCompile(`<\s*(-?\d+),\s*(-?\d+)>.*<\s*(-?\d+),\s*(-?\d+)>`)
	
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

func movePoints(points []Point) {
	for i := range points {
		points[i].x += points[i].vx
		points[i].y += points[i].vy
	}
}

func getBounds(points []Point) (minX, maxX, minY, maxY int) {
	minX, minY = int(1e9), int(1e9)
	maxX, maxY = -int(1e9), -int(1e9)
	
	for _, p := range points {
		if p.x < minX {
			minX = p.x
		}
		if p.x > maxX {
			maxX = p.x
		}
		if p.y < minY {
			minY = p.y
		}
		if p.y > maxY {
			maxY = p.y
		}
	}
	
	return
}

func printMessage(points []Point) {
	minX, maxX, minY, maxY := getBounds(points)
	grid := make([][]bool, maxY-minY+1)
	for i := range grid {
		grid[i] = make([]bool, maxX-minX+1)
	}
	
	for _, p := range points {
		grid[p.y-minY][p.x-minX] = true
	}
	
	for _, row := range grid {
		for _, cell := range row {
			if cell {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
}
