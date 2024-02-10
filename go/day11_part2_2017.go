package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func distance(x, y, z int) int {
	return (abs(x) + abs(y) + abs(z)) / 2
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	input := scanner.Text()

	directions := strings.Split(input, ",")

	x, y, z := 0, 0, 0
	maxDistance := 0

	for _, dir := range directions {
		switch dir {
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

		// Calculate the current distance
		curDistance := distance(x, y, z)
		maxDistance = max(maxDistance, curDistance)
	}

	fmt.Println(maxDistance)
}