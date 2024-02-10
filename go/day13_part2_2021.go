package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	points := make(map[[2]int]struct{})
	folds := make([][2]int, 0)
	scanner := bufio.NewScanner(file)
	readingPoints := true

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			readingPoints = false
			continue
		}
		if readingPoints {
			parts := strings.Split(line, ",")
			x, _ := strconv.Atoi(parts[0])
			y, _ := strconv.Atoi(parts[1])
			points[[2]int{x, y}] = struct{}{}
		} else {
			parts := strings.Split(line, "=")
			val, _ := strconv.Atoi(parts[1])
			if strings.Contains(parts[0], "x") {
				folds = append(folds, [2]int{val, 0})
			} else {
				folds = append(folds, [2]int{0, val})
			}
		}
	}

	for i, fold := range folds {
		newPoints := make(map[[2]int]struct{})
		for point := range points {
			newPoint := [2]int{point[0], point[1]}
			if fold[0] != 0 && point[0] > fold[0] {
				newPoint[0] = fold[0] - (point[0] - fold[0])
			} else if fold[1] != 0 && point[1] > fold[1] {
				newPoint[1] = fold[1] - (point[1] - fold[1])
			}
			newPoints[newPoint] = struct{}{}
		}
		points = newPoints
		if i == 0 {
			fmt.Println("Number of dots visible after first fold:", len(points))
		}
	}

	maxX, maxY := 0, 0
	for point := range points {
		if point[0] > maxX {
			maxX = point[0]
		}
		if point[1] > maxY {
			maxY = point[1]
		}
	}

	grid := make([][]rune, maxY+1)
	for i := range grid {
		grid[i] = make([]rune, maxX+1)
		for j := range grid[i] {
			grid[i][j] = ' '
		}
	}

	for point := range points {
		grid[point[1]][point[0]] = '#'
	}

	for _, row := range grid {
		fmt.Println(string(row))
	}
}