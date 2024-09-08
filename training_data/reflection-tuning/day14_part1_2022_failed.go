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
	scanner := bufio.NewScanner(os.Stdin)
	cave := make(map[Point]bool)
	var maxY int

	// Parse input and build cave structure
	for scanner.Scan() {
		points := strings.Split(scanner.Text(), " -> ")
		for i := 0; i < len(points)-1; i++ {
			start := parsePoint(points[i])
			end := parsePoint(points[i+1])
			drawLine(cave, start, end)
			if start.y > maxY {
				maxY = start.y
			}
			if end.y > maxY {
				maxY = end.y
			}
		}
	}

	sandCount := 0
	for {
		sand := Point{500, 0}
		for {
			if sand.y > maxY {
				// Sand is flowing into the abyss
				fmt.Println(sandCount)
				return
			}
			if !cave[Point{sand.x, sand.y + 1}] {
				sand.y++
			} else if !cave[Point{sand.x - 1, sand.y + 1}] {
				sand.x--
				sand.y++
			} else if !cave[Point{sand.x + 1, sand.y + 1}] {
				sand.x++
				sand.y++
			} else {
				// Sand comes to rest
				cave[sand] = true
				sandCount++
				break
			}
		}
	}
}

func parsePoint(s string) Point {
	coords := strings.Split(s, ",")
	x, _ := strconv.Atoi(coords[0])
	y, _ := strconv.Atoi(coords[1])
	return Point{x, y}
}

func drawLine(cave map[Point]bool, start, end Point) {
	dx := sign(end.x - start.x)
	dy := sign(end.y - start.y)
	for p := start; p != end; p = Point{p.x + dx, p.y + dy} {
		cave[p] = true
	}
	cave[end] = true
}

func sign(x int) int {
	if x > 0 {
		return 1
	} else if x < 0 {
		return -1
	}
	return 0
}
