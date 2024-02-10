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

func (c1 Coord) Substract(c2 Coord) Coord {
	return Coord{c1.X - c2.X, c1.Y - c2.Y}
}

func (c Coord) Opposite() Coord {
	return Coord{-c.X, -c.Y}
}

type Tile byte
type Pipe map[Coord]struct{}

type Grid struct {
	Width  int
	Height int
	Data   map[Coord]Tile
}

var (
	Undefined = Coord{0, 0}
	Top       = Coord{0, -1}
	Right     = Coord{1, 0}
	Bottom    = Coord{0, 1}
	Left      = Coord{-1, 0}
)

const (
	Empty             Tile = '.'
	Start             Tile = 'S'
	Vertical          Tile = '|'
	Horizontal        Tile = '-'
	TopLeftCorner     Tile = 'J'
	TopRightCorner    Tile = 'L'
	BottomLeftCorner  Tile = '7'
	BottomRightCorner Tile = 'F'
	Enclosed          Tile = 'X'
)

var (
	VerticalPipe          = Pipe{Top: struct{}{}, Bottom: struct{}{}}
	HorizontalPipe        = Pipe{Left: struct{}{}, Right: struct{}{}}
	TopLeftCornerPipe     = Pipe{Top: struct{}{}, Left: struct{}{}}
	TopRightCornerPipe    = Pipe{Top: struct{}{}, Right: struct{}{}}
	BottomLeftCornerPipe  = Pipe{Bottom: struct{}{}, Left: struct{}{}}
	BottomRightCornerPipe = Pipe{Bottom: struct{}{}, Right: struct{}{}}
)

var TileToPipe = map[Tile]Pipe{
	Vertical:          VerticalPipe,
	Horizontal:        HorizontalPipe,
	TopLeftCorner:     TopLeftCornerPipe,
	TopRightCorner:    TopRightCornerPipe,
	BottomLeftCorner:  BottomLeftCornerPipe,
	BottomRightCorner: BottomRightCornerPipe,
}

func getPipeFromTile(tile Tile) Pipe {
	if pipe, ok := TileToPipe[tile]; ok {
		return pipe
	}
	return Pipe{}
}

func getTileFromPipe(pipe Pipe) Tile {
	for tile, associatedPipe := range TileToPipe {
		if pipe.isEqualPipe(associatedPipe) {
			return tile
		}
	}

	return Empty
}

func (pipe1 Pipe) isEqualPipe(pipe2 Pipe) bool {
	if len(pipe1) != len(pipe2) {
		return false
	}

	for dir := range pipe1 {
		if _, ok := pipe2[dir]; !ok {
			return false
		}
	}

	return true
}

func buildGrid(input []string) Grid {
	grid := Grid{
		Width:  len(input[0]),
		Height: len(input),
		Data:   map[Coord]Tile{},
	}

	for y, line := range input {
		for x, char := range line {
			if Tile(char) != Empty {
				grid.Data[Coord{x, y}] = Tile(char)
			}
		}
	}

	return grid
}

func (grid Grid) toString() string {
	pipesRepres := map[Tile]string{
		Empty:             " ",
		Start:             "S",
		Vertical:          "║",
		Horizontal:        "═",
		TopLeftCorner:     "╝",
		TopRightCorner:    "╚",
		BottomLeftCorner:  "╗",
		BottomRightCorner: "╔",
		Enclosed:          "X",
	}

	var res string

	for y := 0; y < grid.Height; y++ {
		for x := 0; x < grid.Width; x++ {
			coord := Coord{X: x, Y: y}
			if v, ok := grid.Data[coord]; ok {
				res += pipesRepres[v]
			} else {
				res += pipesRepres[Empty]
			}
		}
		res += "\n"
	}

	return res
}

func findStart(grid Grid) Coord {
	for coord, value := range grid.Data {
		if value == Start {
			return coord
		}
	}
	return Coord{}
}

func (c Coord) getPipeFromNeighbors(grid Grid) Pipe {
	pipe := Pipe{}

	possibleNeighbors := map[Coord]Coord{
		Top:    c.Add(Top),
		Right:  c.Add(Right),
		Bottom: c.Add(Bottom),
		Left:   c.Add(Left),
	}

	for dir := range possibleNeighbors {
		if neighborCoord, ok := possibleNeighbors[dir]; ok {
			neighborPipe := getPipeFromTile(grid.Data[neighborCoord])
			if _, ok := neighborPipe[dir.Opposite()]; ok {
				pipe[dir] = struct{}{}
			}
		}
	}

	return pipe
}

func (start Coord) pathFinding(grid Grid) []Coord {
	path := []Coord{start}
	startPipe := start.getPipeFromNeighbors(grid)

	var previousDir Coord
	var current Coord
	for dir := range startPipe {
		previousDir = dir
		current = start.Add(dir)
	}

	for current != start {
		path = append(path, current)
		currentPipe := getPipeFromTile(grid.Data[current])
		for dir := range currentPipe {
			if dir != previousDir.Opposite() {
				previousDir = dir
				current = current.Add(dir)
				break
			}
		}
	}

	return path
}

func getPathGrid(grid Grid, path []Coord, empty Tile) Grid {
	newGrid := Grid{
		Width:  grid.Width,
		Height: grid.Height,
		Data:   map[Coord]Tile{},
	}

	for _, coord := range path {
		newGrid.Data[coord] = grid.Data[coord]
	}

	start := path[0]
	newGrid.Data[start] = getTileFromPipe(start.getPipeFromNeighbors(grid))

	return newGrid
}

func (c Coord) isInside(grid Grid, empty Tile) bool {
	if _, ok := grid.Data[c]; ok {
		return false
	}

	startPipe := empty
	numPipeOnLeft := 0
	for x := 0; x < c.X; x++ {
		coord := Coord{x, c.Y}
		v := grid.Data[coord]

		switch v {
		case Vertical:
			numPipeOnLeft++
		case TopRightCorner:
			startPipe = TopRightCorner
		case BottomRightCorner:
			startPipe = BottomRightCorner
		case TopLeftCorner:
			if startPipe == BottomRightCorner {
				startPipe = empty
				numPipeOnLeft++
			} else if v == TopRightCorner {
				startPipe = Empty
			}
		case BottomLeftCorner:
			if startPipe == TopRightCorner {
				startPipe = Empty
				numPipeOnLeft++
			} else if startPipe == BottomRightCorner {
				startPipe = Empty
			}
		}
	}

	return numPipeOnLeft%2 == 1
}

func solve(input []string) int {
	grid := buildGrid(input)
	start := findStart(grid)
	path := start.pathFinding(grid)
	pathGrid := getPathGrid(grid, path, Empty)

	cnt := 0
	for y := 0; y < grid.Height; y++ {
		for x := 0; x < grid.Width; x++ {
			c := Coord{X: x, Y: y}
			if c.isInside(pathGrid, Empty) {
				cnt++
			}
		}
	}

	return cnt
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