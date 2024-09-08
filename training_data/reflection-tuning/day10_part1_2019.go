package main

import (
	"bufio"
	"fmt"
	"os"
)

type Point struct {
	x, y int
}

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func countVisibleAsteroids(asteroids []Point, station Point) int {
	slopes := make(map[Point]bool)
	for _, asteroid := range asteroids {
		if asteroid == station {
			continue
		}
		dx, dy := asteroid.x-station.x, asteroid.y-station.y
		g := gcd(abs(dx), abs(dy))
		slopes[Point{dx / g, dy / g}] = true
	}
	return len(slopes)
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	var asteroids []Point
	scanner := bufio.NewScanner(file)
	for y := 0; scanner.Scan(); y++ {
		line := scanner.Text()
		for x, char := range line {
			if char == '#' {
				asteroids = append(asteroids, Point{x, y})
			}
		}
	}

	maxVisible := 0
	for _, station := range asteroids {
		visible := countVisibleAsteroids(asteroids, station)
		if visible > maxVisible {
			maxVisible = visible
		}
	}

	fmt.Println(maxVisible)
}
