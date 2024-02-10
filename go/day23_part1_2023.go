package main

import (
	"fmt"
	"os"
	"strings"
)

type Coord struct {
	x int
	y int
}

func (c1 Coord) Add(c2 Coord) Coord {
	return Coord{c1.x + c2.x, c1.y + c2.y}
}

type Grid struct {
	width  int
	height int
	data   map[Coord]byte
}

var (
	North = Coord{0, -1}
	South = Coord{0, 1}
	West  = Coord{-1, 0}
	East  = Coord{1, 0}
)

const (
	Empty       byte = '.'
	Wall        byte = '#'
	NorthSlopes byte = '^'
	SouthSlopes byte = 'v'
	WestSlopes  byte = '<'
	EastSlopes  byte = '>'
)

var SlopeToDir = map[byte]Coord{
	NorthSlopes: North,
	SouthSlopes: South,
	WestSlopes:  West,
	EastSlopes:  East,
}

type Edge struct {
	start  Coord
	end    Coord
	weight int
}

type Graph struct {
	vertices map[Coord]struct{}
	edges    map[Coord]map[Edge]struct{}
}

func isInBounds(grid Grid, coord Coord) bool {
	return 0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height
}

func parseInput(input []string) Grid {
	grid := Grid{
		width:  len(input[0]),
		height: len(input),
		data:   make(map[Coord]byte),
	}

	for y, line := range input {
		for x, char := range line {
			if byte(char) != Empty {
				grid.data[Coord{x, y}] = byte(char)
			}
		}
	}

	return grid
}

func isValidNeighbor(grid Grid, coord, dir Coord) bool {
	if !isInBounds(grid, coord) {
		return false
	}
	if grid.data[coord] == Wall {
		return false
	}
	return true
}

func isValidNeighborWithSlopes(grid Grid, coord, dir Coord) bool {
	if !isInBounds(grid, coord) {
		return false
	}
	if _, ok := grid.data[coord]; !ok {
		return true
	}
	if grid.data[coord] == Wall {
		return false
	}
	return SlopeToDir[grid.data[coord]] == dir
}

func neighbors4(grid Grid, coord Coord, isValidNeighborFunc func(Grid, Coord, Coord) bool) []Coord {
	directions := []Coord{North, South, West, East}
	validNeighbors := []Coord{}

	for _, dir := range directions {
		neighbor := coord.Add(dir)
		if isValidNeighborFunc(grid, neighbor, dir) {
			validNeighbors = append(validNeighbors, neighbor)
		}
	}

	return validNeighbors
}

func getGraph(grid Grid, start Coord, end Coord, isValidNeighborFunc func(Grid, Coord, Coord) bool) Graph {
	graph := Graph{
		vertices: map[Coord]struct{}{
			start: {},
			end:   {},
		},
		edges: map[Coord]map[Edge]struct{}{},
	}

	for y := 0; y < grid.height; y++ {
		for x := 0; x < grid.width; x++ {
			coord := Coord{x, y}
			if _, ok := grid.data[coord]; !ok {
				if len(neighbors4(grid, coord, isValidNeighbor)) > 2 {
					graph.vertices[coord] = struct{}{}
				}
			}
		}
	}

	for start := range graph.vertices {
		edges := getEdgesBFS(grid, start, graph.vertices, isValidNeighborFunc)
		graph.edges[start] = edges
	}

	return graph
}

func getEdgesBFS(grid Grid, start Coord, vertices map[Coord]struct{}, isValidNeighborFunc func(Grid, Coord, Coord) bool) map[Edge]struct{} {
	frontier := []Coord{start}
	reached := map[Coord]struct{}{start: {}}
	distances := map[Coord]int{start: 0}
	edges := map[Edge]struct{}{}

	for len(frontier) > 0 {
		current := frontier[0]
		frontier = frontier[1:]

		if _, ok := vertices[current]; ok && current != start {
			edge := Edge{
				start:  start,
				end:    current,
				weight: distances[current],
			}
			edges[edge] = struct{}{}
			continue
		}

		for _, next := range neighbors4(grid, current, isValidNeighborFunc) {
			if _, ok := reached[next]; !ok {
				frontier = append(frontier, next)
				reached[next] = struct{}{}
				distances[next] = distances[current] + 1
			}
		}
	}

	return edges
}

func getMaxDistanceDFS(grid Grid, graph Graph, current Coord, end Coord, seen map[Coord]struct{}) (bool, int) {
	if current == end {
		return true, 0
	}

	maxi := 0
	seen[current] = struct{}{}
	for edge := range graph.edges[current] {
		if _, ok := seen[edge.end]; !ok {
			isValid, dist := getMaxDistanceDFS(grid, graph, edge.end, end, seen)
			if isValid {
				maxi = max(maxi, dist+edge.weight)
			}
		}
	}
	delete(seen, current)

	if maxi == 0 {
		return false, 0
	}
	return true, maxi
}

func solve(input []string) int {
	grid := parseInput(input)

	start := Coord{1, 0}
	end := Coord{grid.width - 2, grid.height - 1}

	graph := getGraph(grid, start, end, isValidNeighborWithSlopes)

	_, maxDist := getMaxDistanceDFS(grid, graph, start, end, map[Coord]struct{}{})
	return maxDist
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