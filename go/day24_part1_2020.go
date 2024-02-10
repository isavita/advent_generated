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

	count := 0
	for _, black := range blackTiles {
		if black {
			count++
		}
	}
	fmt.Println(count)
}