package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"sort"
)

type Point struct {
	x, y int
}

type Asteroid struct {
	Point
	angle, distance float64
}

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func countVisibleAsteroids(asteroids []Point, station Point) int {
	visible := make(map[Point]bool)
	for _, asteroid := range asteroids {
		if asteroid != station {
			dx, dy := asteroid.x-station.x, asteroid.y-station.y
			g := gcd(abs(dx), abs(dy))
			visible[Point{dx / g, dy / g}] = true
		}
	}
	return len(visible)
}

func findBestLocation(asteroids []Point) (Point, int) {
	var bestLocation Point
	maxVisible := 0
	for _, station := range asteroids {
		visible := countVisibleAsteroids(asteroids, station)
		if visible > maxVisible {
			maxVisible = visible
			bestLocation = station
		}
	}
	return bestLocation, maxVisible
}

func vaporizeAsteroids(asteroids []Point, station Point) []Asteroid {
	var targets []Asteroid
	for _, asteroid := range asteroids {
		if asteroid != station {
			dx, dy := float64(asteroid.x-station.x), float64(asteroid.y-station.y)
			angle := math.Atan2(dx, -dy)
			if angle < 0 {
				angle += 2 * math.Pi
			}
			distance := math.Sqrt(dx*dx + dy*dy)
			targets = append(targets, Asteroid{asteroid, angle, distance})
		}
	}

	sort.Slice(targets, func(i, j int) bool {
		if targets[i].angle == targets[j].angle {
			return targets[i].distance < targets[j].distance
		}
		return targets[i].angle < targets[j].angle
	})

	var vaporized []Asteroid
	for len(targets) > 0 {
		var remaining []Asteroid
		var lastAngle float64 = -1
		for _, target := range targets {
			if target.angle != lastAngle {
				vaporized = append(vaporized, target)
				lastAngle = target.angle
			} else {
				remaining = append(remaining, target)
			}
		}
		targets = remaining
	}

	return vaporized
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
		panic(err)
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

	bestLocation, maxVisible := findBestLocation(asteroids)
	fmt.Printf("Part 1: Best location is (%d,%d) with %d asteroids detected\n", bestLocation.x, bestLocation.y, maxVisible)

	vaporized := vaporizeAsteroids(asteroids, bestLocation)
	if len(vaporized) >= 200 {
		asteroid200 := vaporized[199]
		score := asteroid200.x*100 + asteroid200.y
		fmt.Printf("Part 2: 200th asteroid vaporized is (%d,%d) with score %d\n", asteroid200.x, asteroid200.y, score)
	}
}
