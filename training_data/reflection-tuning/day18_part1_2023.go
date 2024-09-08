package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var points []Point
	current := Point{0, 0}
	perimeter := 0

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Fields(line)
		direction, distanceStr := parts[0], parts[1]
		distance, _ := strconv.Atoi(distanceStr)

		perimeter += distance

		switch direction {
		case "U":
			current.y -= distance
		case "D":
			current.y += distance
		case "L":
			current.x -= distance
		case "R":
			current.x += distance
		}

		points = append(points, current)
	}

	area := shoelaceFormula(points)
	interiorPoints := picksTheorem(area, perimeter)
	totalCubicMeters := interiorPoints + perimeter

	fmt.Println(totalCubicMeters)
}

func shoelaceFormula(points []Point) int {
	area := 0
	j := len(points) - 1
	for i := 0; i < len(points); i++ {
		area += (points[j].x + points[i].x) * (points[j].y - points[i].y)
		j = i
	}
	return abs(area) / 2
}

func picksTheorem(area, perimeter int) int {
	return area - perimeter/2 + 1
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}
