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
	points := readInput()
	
	seconds := 0
	minArea := calculateArea(points)
	
	for {
		for i := range points {
			points[i].x += points[i].vx
			points[i].y += points[i].vy
		}
		seconds++
		
		area := calculateArea(points)
		if area < minArea {
			minArea = area
		} else {
			// Revert the last move
			for i := range points {
				points[i].x -= points[i].vx
				points[i].y -= points[i].vy
			}
			seconds--
			break
		}
	}
	
	printMessage(points)
	fmt.Printf("Seconds: %d\n", seconds)
}

func readInput() []Point {
	var points []Point
	scanner := bufio.NewScanner(os.Stdin)
	re := regexp.MustCompile(`position=<\s*(-?\d+),\s*(-?\d+)>\s*velocity=<\s*(-?\d+),\s*(-?\d+)>`)
	
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

func calculateArea(points []Point) int {
	minX, maxX := points[0].x, points[0].x
	minY, maxY := points[0].y, points[0].y
	
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
	
	return (maxX - minX) * (maxY - minY)
}

func printMessage(points []Point) {
	minX, maxX := points[0].x, points[0].x
	minY, maxY := points[0].y, points[0].y
	
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
