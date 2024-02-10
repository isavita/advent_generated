package main

import (
	"bufio"
	"fmt"
	"os"
)

type Coordinate struct {
	q, r int
}

var directions = map[string]Coordinate{
	"e":  {1, 0},
	"se": {0, 1},
	"sw": {-1, 1},
	"w":  {-1, 0},
	"nw": {0, -1},
	"ne": {1, -1},
}

func getNeighbors(tile Coordinate) []Coordinate {
	var neighbors []Coordinate
	for _, dir := range directions {
		neighbors = append(neighbors, Coordinate{tile.q + dir.q, tile.r + dir.r})
	}
	return neighbors
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	blackTiles := make(map[Coordinate]bool)
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		coord := Coordinate{0, 0}
		for i := 0; i < len(line); i++ {
			var dir string
			switch line[i] {
			case 'e', 'w':
				dir = string(line[i])
			case 'n', 's':
				dir = string(line[i : i+2])
				i++
			}
			move := directions[dir]
			coord.q += move.q
			coord.r += move.r
		}
		blackTiles[coord] = !blackTiles[coord]
	}

	for day := 0; day < 100; day++ {
		tilesToCheck := make(map[Coordinate]bool)
		for tile := range blackTiles {
			if blackTiles[tile] {
				tilesToCheck[tile] = true
				for _, neighbor := range getNeighbors(tile) {
					tilesToCheck[neighbor] = true
				}
			}
		}

		newBlackTiles := make(map[Coordinate]bool)
		for tile := range tilesToCheck {
			blackNeighborCount := 0
			for _, neighbor := range getNeighbors(tile) {
				if blackTiles[neighbor] {
					blackNeighborCount++
				}
			}
			if blackTiles[tile] && (blackNeighborCount == 1 || blackNeighborCount == 2) {
				newBlackTiles[tile] = true
			} else if !blackTiles[tile] && blackNeighborCount == 2 {
				newBlackTiles[tile] = true
			}
		}

		blackTiles = newBlackTiles
	}

	fmt.Println(len(blackTiles))
}