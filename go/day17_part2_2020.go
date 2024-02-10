package main

import (
	"fmt"
	"os"
	"strings"
)

type Coordinate4D struct {
	x, y, z, w int
}

func main() {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("File reading error:", err)
		return
	}

	initialState := strings.Split(strings.TrimSpace(string(input)), "\n")
	activeCubes := make(map[Coordinate4D]bool)

	for y, line := range initialState {
		for x, char := range line {
			if char == '#' {
				activeCubes[Coordinate4D{x, y, 0, 0}] = true
			}
		}
	}

	for cycle := 0; cycle < 6; cycle++ {
		activeCubes = simulateCycle4D(activeCubes)
	}

	fmt.Println(len(activeCubes))
}

func simulateCycle4D(activeCubes map[Coordinate4D]bool) map[Coordinate4D]bool {
	newActiveCubes := make(map[Coordinate4D]bool)
	neighborCounts := make(map[Coordinate4D]int)

	for coord := range activeCubes {
		for dw := -1; dw <= 1; dw++ {
			for dz := -1; dz <= 1; dz++ {
				for dy := -1; dy <= 1; dy++ {
					for dx := -1; dx <= 1; dx++ {
						if dw == 0 && dz == 0 && dy == 0 && dx == 0 {
							continue
						}
						neighbor := Coordinate4D{coord.x + dx, coord.y + dy, coord.z + dz, coord.w + dw}
						neighborCounts[neighbor]++
					}
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