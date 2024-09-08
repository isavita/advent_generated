package main

import (
	"fmt"
	"strings"
)

func main() {
	input := "ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne" // Example input, replace with actual input

	steps := strings.Split(input, ",")
	x, y, z := 0, 0, 0
	maxDistance := 0

	for _, step := range steps {
		switch step {
		case "n":
			y++
			z--
		case "s":
			y--
			z++
		case "ne":
			x++
			z--
		case "sw":
			x--
			z++
		case "nw":
			x--
			y++
		case "se":
			x++
			y--
		}

		distance := (abs(x) + abs(y) + abs(z)) / 2
		if distance > maxDistance {
			maxDistance = distance
		}
	}

	finalDistance := (abs(x) + abs(y) + abs(z)) / 2
	fmt.Printf("Part 1: Final distance is %d steps\n", finalDistance)
	fmt.Printf("Part 2: Furthest distance reached is %d steps\n", maxDistance)
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}
