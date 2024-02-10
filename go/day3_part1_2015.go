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
	x, y := 0, 0 // Santa's starting position

	// Mark the starting location as visited
	visitedHouses[[2]int{x, y}] = true

	for _, dir := range directions {
		switch dir {
		case '^':
			y++ // Move north
		case 'v':
			y-- // Move south
		case '>':
			x++ // Move east
		case '<':
			x-- // Move west
		}

		// Mark the new location as visited
		visitedHouses[[2]int{x, y}] = true
	}

	fmt.Println(len(visitedHouses))
}