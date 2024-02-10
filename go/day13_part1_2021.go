package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	X, Y int
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	// Read file and store data
	scanner := bufio.NewScanner(file)
	points := map[Point]bool{}
	var folds []string
	readingPoints := true
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			readingPoints = false
			continue
		}
		if readingPoints {
			coords := strings.Split(line, ",")
			x, _ := strconv.Atoi(coords[0])
			y, _ := strconv.Atoi(coords[1])
			points[Point{X: x, Y: y}] = true
		} else {
			folds = append(folds, line)
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	// Process the first fold instruction
	fold := strings.Fields(folds[0])[2] // "fold along x=5" -> "x=5"
	axisValue := strings.Split(fold, "=")
	axis := axisValue[0]
	value, _ := strconv.Atoi(axisValue[1])

	newPoints := map[Point]bool{}
	if axis == "x" {
		for point := range points {
			if point.X > value {
				point.X = 2*value - point.X
			}
			newPoints[point] = true
		}
	} else {
		for point := range points {
			if point.Y > value {
				point.Y = 2*value - point.Y
			}
			newPoints[point] = true
		}
	}

	fmt.Println(len(newPoints))
}