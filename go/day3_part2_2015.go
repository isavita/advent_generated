package main

import (
	"fmt"
	"log"
	"os"
)

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	directions := string(data)
	visitedHouses := make(map[[2]int]bool)
	xSanta, ySanta := 0, 0 // Santa's starting position
	xRobo, yRobo := 0, 0   // Robo-Santa's starting position
	isSantaTurn := true    // Flag to alternate turns between Santa and Robo-Santa

	// Mark the starting location as visited by both
	visitedHouses[[2]int{xSanta, ySanta}] = true

	for _, dir := range directions {
		var x, y *int
		if isSantaTurn {
			x, y = &xSanta, &ySanta
		} else {
			x, y = &xRobo, &yRobo
		}

		switch dir {
		case '^':
			*y++ // Move north
		case 'v':
			*y-- // Move south
		case '>':
			*x++ // Move east
		case '<':
			*x-- // Move west
		}

		// Mark the new location as visited
		visitedHouses[[2]int{*x, *y}] = true
		isSantaTurn = !isSantaTurn // Switch turns
	}

	fmt.Println(len(visitedHouses))
}