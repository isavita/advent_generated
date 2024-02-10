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

type Beam struct {
	Origin Coord
	Dir    Coord
}

const (
	Empty              byte = '.'
	AscendingMirror    byte = '/'
	DescendingMirror   byte = '\\'
	VerticalSplitter   byte = '|'
	HorizontalSplitter byte = '-'
)

var (
	North = Coord{0, -1}
	West  = Coord{-1, 0}
	South = Coord{0, 1}
	East  = Coord{1, 0}
)

func (coord Coord) rotate90() Coord {
	return Coord{coord.Y, -coord.X}
}

func (coord Coord) rotateNeg90() Coord {
	return Coord{-coord.Y, coord.X}
}

func (coord Coord) isInBounds(grid Grid) bool {
	return 0 <= coord.X && coord.X < grid.Width && 0 <= coord.Y && coord.Y < grid.Height
}

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

func nextBeam(grid Grid, beam Beam) []Beam {
	beams := []Beam{}
	char, ok := grid.Data[beam.Origin]

	if !ok {
		newBeam := Beam{
			Origin: beam.Origin.Add(beam.Dir),
			Dir:    beam.Dir,
		}
		return append(beams, newBeam)
	}

	switch {
	case char == AscendingMirror:
		var newDir Coord
		if beam.Dir == North || beam.Dir == South {
			newDir = beam.Dir.rotateNeg90()
		} else {
			newDir = beam.Dir.rotate90()
		}
		newBeam := Beam{
			Origin: beam.Origin.Add(newDir),
			Dir:    newDir,
		}
		beams = append(beams, newBeam)

	case char == DescendingMirror:
		var newDir Coord
		if beam.Dir == North || beam.Dir == South {
			newDir = beam.Dir.rotate90()
		} else {
			newDir = beam.Dir.rotateNeg90()
		}
		newBeam := Beam{
			Origin: beam.Origin.Add(newDir),
			Dir:    newDir,
		}
		beams = append(beams, newBeam)

	case char == VerticalSplitter && (beam.Dir == East || beam.Dir == West):
		newDir1 := beam.Dir.rotate90()
		newBeam1 := Beam{
			Origin: beam.Origin.Add(newDir1),
			Dir:    newDir1,
		}
		newDir2 := beam.Dir.rotateNeg90()
		newBeam2 := Beam{
			Origin: beam.Origin.Add(newDir2),
			Dir:    newDir2,
		}
		beams = append(beams, newBeam1, newBeam2)

	case char == HorizontalSplitter && (beam.Dir == North || beam.Dir == South):
		newDir1 := beam.Dir.rotate90()
		newBeam1 := Beam{
			Origin: beam.Origin.Add(newDir1),
			Dir:    newDir1,
		}
		newDir2 := beam.Dir.rotateNeg90()
		newBeam2 := Beam{
			Origin: beam.Origin.Add(newDir2),
			Dir:    newDir2,
		}
		beams = append(beams, newBeam1, newBeam2)

	default:
		newBeam := Beam{
			Origin: beam.Origin.Add(beam.Dir),
			Dir:    beam.Dir,
		}
		beams = append(beams, newBeam)
	}

	return beams
}

func calculatePropagation(grid Grid, start Beam) map[Beam]struct{} {
	alreadySeen := map[Beam]struct{}{}
	toExplore := []Beam{start}

	for len(toExplore) > 0 {
		beam := toExplore[0]
		toExplore = toExplore[1:]

		_, isAlreadySeen := alreadySeen[beam]
		if beam.Origin.isInBounds(grid) && !isAlreadySeen {
			alreadySeen[beam] = struct{}{}
			toExplore = append(toExplore, nextBeam(grid, beam)...)
		}
	}

	return alreadySeen
}

func buildBeamGrid(grid Grid, alreadySeen map[Beam]struct{}) Grid {
	beamGrid := Grid{
		Width:  grid.Width,
		Height: grid.Height,
		Data:   make(map[Coord]byte, len(grid.Data)+len(alreadySeen)),
	}

	for coord, char := range grid.Data {
		beamGrid.Data[coord] = char
	}

	for beam := range alreadySeen {
		if _, ok := grid.Data[beam.Origin]; !ok {
			if _, ok := beamGrid.Data[beam.Origin]; ok {
				beamGrid.Data[beam.Origin] = '2'
			} else {
				switch beam.Dir {
				case North:
					beamGrid.Data[beam.Origin] = '^'
				case East:
					beamGrid.Data[beam.Origin] = '>'
				case South:
					beamGrid.Data[beam.Origin] = 'v'
				case West:
					beamGrid.Data[beam.Origin] = '<'
				}
			}
		}
	}

	return beamGrid
}

func calculateEnergization(alreadySeen map[Beam]struct{}) map[Coord]struct{} {
	alreadyEnergized := map[Coord]struct{}{}

	for beam := range alreadySeen {
		if _, ok := alreadyEnergized[beam.Origin]; !ok {
			alreadyEnergized[beam.Origin] = struct{}{}
		}
	}

	return alreadyEnergized
}

func getBorder(grid Grid) []Beam {
	border := []Beam{}

	for x := 0; x < grid.Width; x++ {
		coord := Coord{x, 0}
		beam := Beam{coord, South}
		border = append(border, beam)

		coord = Coord{x, grid.Height - 1}
		beam = Beam{coord, North}
		border = append(border, beam)
	}

	for y := 0; y < grid.Height; y++ {
		coord := Coord{0, y}
		beam := Beam{coord, East}
		border = append(border, beam)

		coord = Coord{grid.Width - 1, y}
		beam = Beam{coord, West}
		border = append(border, beam)
	}

	return border
}

func solve(input []string) int {
	grid := buildGrid(input)
	start := Beam{Coord{0, 0}, East}

	alreadySeen := calculatePropagation(grid, start)
	alreadyEnergized := calculateEnergization(alreadySeen)

	res := len(alreadyEnergized)
	return res
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