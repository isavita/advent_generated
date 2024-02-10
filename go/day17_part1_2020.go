package main

import (
	"fmt"
	"os"
	"strings"
)

type Coordinate struct {
	x, y, z int
}

func main() {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("File reading error:", err)
		return
	}

	initialState := strings.Split(strings.TrimSpace(string(input)), "\n")
	activeCubes := make(map[Coordinate]bool)

	for y, line := range initialState {
		for x, char := range line {
			if char == '#' {
				activeCubes[Coordinate{x, y, 0}] = true
			}
		}
	}

	for cycle := 0; cycle < 6; cycle++ {
		activeCubes = simulateCycle(activeCubes)
	}

	fmt.Println(len(activeCubes))
}

func simulateCycle(activeCubes map[Coordinate]bool) map[Coordinate]bool {
	newActiveCubes := make(map[Coordinate]bool)
	neighborCounts := make(map[Coordinate]int)

	for coord := range activeCubes {
		for dz := -1; dz <= 1; dz++ {
			for dy := -1; dy <= 1; dy++ {
				for dx := -1; dx <= 1; dx++ {
					if dz == 0 && dy == 0 && dx == 0 {
						continue
					}
					neighbor := Coordinate{coord.x + dx, coord.y + dy, coord.z + dz}
					neighborCounts[neighbor]++
				}
			}
		}
	}

	for coord, count := range neighborCounts {
		if count == 3 || (count == 2 && activeCubes[coord]) {
			newActiveCubes[coord] = true
		}
	}

	return newActiveCubes
}