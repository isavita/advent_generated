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

func parseInput(input []string) map[Point]bool {
	cave := make(map[Point]bool)
	for _, line := range input {
		points := strings.Split(line, " -> ")
		for i := 0; i < len(points)-1; i++ {
			start := parsePoint(points[i])
			end := parsePoint(points[i+1])
			drawLine(cave, start, end)
		}
	}
	return cave
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
	if x < 0 {
		return -1
	}
	if x > 0 {
		return 1
	}
	return 0
}

func simulateSand(cave map[Point]bool, maxY int, hasFloor bool) int {
	count := 0
	for {
		sand := Point{500, 0}
		for {
			if !hasFloor && sand.y > maxY {
				return count
			}
			if hasFloor && sand.y == maxY+1 {
				cave[sand] = true
				count++
				break
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
				cave[sand] = true
				count++
				if sand.x == 500 && sand.y == 0 {
					return count
				}
				break
			}
		}
	}
}

func findMaxY(cave map[Point]bool) int {
	maxY := 0
	for p := range cave {
		if p.y > maxY {
			maxY = p.y
		}
	}
	return maxY
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	var input []string
	for scanner.Scan() {
		input = append(input, scanner.Text())
	}

	cave := parseInput(input)
	maxY := findMaxY(cave)

	part1 := simulateSand(cave, maxY, false)
	fmt.Println("Part 1:", part1)

	cave = parseInput(input) // Reset cave for part 2
	part2 := simulateSand(cave, maxY, true)
	fmt.Println("Part 2:", part2)
}
