package main

import (
	"bufio"
	"fmt"
	"os"
)

type Point struct {
	x, y int
}

func main() {
	universe := readInput()
	galaxies := findGalaxies(universe)
	
	fmt.Println("Part 1:", sumShortestPaths(galaxies, universe, 2))
	fmt.Println("Part 2:", sumShortestPaths(galaxies, universe, 1000000))
}

func readInput() []string {
	var universe []string
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		universe = append(universe, scanner.Text())
	}
	return universe
}

func findGalaxies(universe []string) []Point {
	var galaxies []Point
	for y, row := range universe {
		for x, char := range row {
			if char == '#' {
				galaxies = append(galaxies, Point{x, y})
			}
		}
	}
	return galaxies
}

func sumShortestPaths(galaxies []Point, universe []string, expansionFactor int) int {
	emptyRows := make([]bool, len(universe))
	emptyCols := make([]bool, len(universe[0]))

	for y, row := range universe {
		emptyRows[y] = true
		for x, char := range row {
			if char == '#' {
				emptyRows[y] = false
				emptyCols[x] = false
			}
		}
	}

	sum := 0
	for i := 0; i < len(galaxies); i++ {
		for j := i + 1; j < len(galaxies); j++ {
			sum += shortestPath(galaxies[i], galaxies[j], emptyRows, emptyCols, expansionFactor)
		}
	}
	return sum
}

func shortestPath(a, b Point, emptyRows, emptyCols []bool, expansionFactor int) int {
	distance := 0
	for x := min(a.x, b.x); x < max(a.x, b.x); x++ {
		if emptyCols[x] {
			distance += expansionFactor
		} else {
			distance++
		}
	}
	for y := min(a.y, b.y); y < max(a.y, b.y); y++ {
		if emptyRows[y] {
			distance += expansionFactor
		} else {
			distance++
		}
	}
	return distance
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
