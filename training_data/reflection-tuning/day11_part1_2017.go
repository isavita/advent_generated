package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"strings"
)

func main() {
	// Read input
	input, err := ioutil.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}

	// Parse steps
	steps := strings.Split(strings.TrimSpace(string(input)), ",")

	// Initialize position
	x, y, z := 0, 0, 0

	// Process steps
	for _, step := range steps {
		switch step {
		case "n":
			y++
			z--
		case "ne":
			x++
			z--
		case "se":
			x++
			y--
		case "s":
			y--
			z++
		case "sw":
			x--
			z++
		case "nw":
			x--
			y++
		}
	}

	// Calculate shortest path
	distance := math.Max(math.Abs(float64(x)), math.Max(math.Abs(float64(y)), math.Abs(float64(z))))

	fmt.Printf("Fewest steps required: %.0f\n", distance)
}
