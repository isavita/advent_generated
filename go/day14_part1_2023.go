package main

import (
	"fmt"
	"os"
	"strings"
)

type Coord struct {
	X int
	Y int
}

func (c1 Coord) Add(c2 Coord) Coord {
	return Coord{c1.X + c2.X, c1.Y + c2.Y}
}

type Grid struct {
	Width  int
	Height int
	Data   map[Coord]byte
}

func (coord Coord) isInBounds(grid Grid) bool {
	return 0 <= coord.X && coord.X < grid.Width && 0 <= coord.Y && coord.Y < grid.Height
}

const (
	Empty     byte = '.'
	CubicRock byte = '#'
	RoundRock byte = 'O'
)

var (
	North = Coord{0, -1}
	West  = Coord{-1, 0}
	South = Coord{0, 1}
	East  = Coord{1, 0}
)

func buildGrid(input []string) Grid {
	grid := Grid{
		Width:  len(input[0]),
		Height: len(input),
		Data:   map[Coord]byte{},
	}

	for y, line := range input {
		for x, char := range line {
			if byte(char) != Empty {
				grid.Data[Coord{x, y}] = byte(char)
			}
		}
	}

	return grid
}

func (grid Grid) toString() string {
	var result string

	for y := 0; y < grid.Height; y++ {
		for x := 0; x < grid.Width; x++ {
			coord := Coord{X: x, Y: y}
			if v, ok := grid.Data[coord]; ok {
				result += string(v)
			} else {
				result += string(Empty)
			}
		}
		result += "\n"
	}

	return result
}

func shiftSingleRock(grid Grid, coord Coord, dir Coord) {
	if grid.Data[coord] == RoundRock {
		current := coord
		before := coord.Add(dir)

		_, ok := grid.Data[before]
		for !ok && before.isInBounds(grid) {
			grid.Data[before] = RoundRock
			delete(grid.Data, current)

			current = before
			before = before.Add(dir)
			_, ok = grid.Data[before]
		}
	}
}

func shiftRocks(grid Grid, dir Coord) {
	switch dir {
	case North, West:
		for x := 0; x < grid.Width; x++ {
			for y := 0; y < grid.Height; y++ {
				shiftSingleRock(grid, Coord{x, y}, dir)
			}
		}

	case South, East:
		for x := grid.Width - 1; x >= 0; x-- {
			for y := grid.Height - 1; y >= 0; y-- {
				shiftSingleRock(grid, Coord{x, y}, dir)
			}
		}
	}
}

func calculateLoad(grid Grid) int {
	load := 0

	for x := 0; x < grid.Width; x++ {
		for y := 0; y < grid.Height; y++ {
			coord := Coord{x, y}
			if grid.Data[coord] == RoundRock {
				load += grid.Height - y
			}
		}
	}

	return load
}

func solve(input []string) int {
	grid := buildGrid(input)
	shiftRocks(grid, North)

	return calculateLoad(grid)
}

func readFile(fileName string) []string {
	file, err := os.ReadFile(fileName)
	if err != nil {
		panic(err)
	}

	return strings.Split(strings.TrimSpace(string(file)), "\n")
}

func main() {
	input := readFile("input.txt")
	fmt.Println(solve(input))
}