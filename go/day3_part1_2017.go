package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	// Step 1: Read input
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}

	target, err := strconv.Atoi(strings.TrimSpace(string(data)))
	if err != nil {
		fmt.Println("File parsing error", err)
		return
	}

	// Step 2: Find the square
	sideLength := int(math.Ceil(math.Sqrt(float64(target))))
	if sideLength%2 == 0 {
		sideLength++
	}

	// Step 3: Find distance to the nearest middle point
	maxValue := sideLength * sideLength
	stepsFromEdge := (sideLength - 1) / 2
	var distanceToMiddle int

	for i := 0; i < 4; i++ {
		middlePoint := maxValue - stepsFromEdge - (sideLength-1)*i
		distance := int(math.Abs(float64(target - middlePoint)))
		if distance < distanceToMiddle || i == 0 {
			distanceToMiddle = distance
		}
	}

	// Step 4: Calculate Manhattan Distance
	manhattanDistance := stepsFromEdge + distanceToMiddle

	fmt.Println(manhattanDistance)
}