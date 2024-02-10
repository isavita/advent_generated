package main

import (
	"fmt"
	"os"
	"strings"
)

type Vertice string

type Edge struct {
	start  Vertice
	end    Vertice
	weight int
}

type Graph map[Vertice]map[Edge]struct{}

func parseInput(input []string) Graph {
	weight := 1

	graph := Graph{}

	for _, line := range input {
		parts := strings.Split(line, ": ")
		vertice := Vertice(parts[0])
		others := strings.Split(parts[1], " ")

		if _, ok := graph[vertice]; !ok {
			graph[vertice] = map[Edge]struct{}{}
		}

		for _, other := range others {
			otherVertice := Vertice(other)
			if _, ok := graph[otherVertice]; !ok {
				graph[otherVertice] = map[Edge]struct{}{}
			}

			graph[vertice][Edge{vertice, otherVertice, weight}] = struct{}{}
			graph[otherVertice][Edge{otherVertice, vertice, weight}] = struct{}{}
		}
	}

	return graph
}

func breadthFirstSearch(graph Graph, start Vertice, goalFunc func(Vertice) bool) (bool, map[Vertice]Vertice) {
	frontier := []Vertice{start}
	reached := map[Vertice]struct{}{start: {}}
	cameFrom := map[Vertice]Vertice{start: start}

	for len(frontier) > 0 {
		current := frontier[0]
		frontier = frontier[1:]

		if goalFunc(current) {
			return true, cameFrom
		}

		for next := range graph[current] {
			if _, ok := reached[next.end]; !ok {
				frontier = append(frontier, next.end)
				reached[next.end] = struct{}{}
				cameFrom[next.end] = current
			}
		}
	}

	return false, cameFrom
}

func reconstructPath(start Vertice, end Vertice, cameFrom map[Vertice]Vertice) []Vertice {
	path := []Vertice{}
	current := end
	for current != start {
		path = append([]Vertice{current}, path...)
		current = cameFrom[current]
	}
	path = append([]Vertice{start}, path...)
	return path
}

func copyGraph(graph Graph) Graph {
	newGraph := Graph{}
	for vertice, edges := range graph {
		newGraph[vertice] = map[Edge]struct{}{}
		for edge := range edges {
			newGraph[vertice][edge] = struct{}{}
		}
	}
	return newGraph
}

func solve(input []string) int {
	minCut := 3

	graph := parseInput(input)

	var source Vertice
	for vertice := range graph {
		source = vertice
		break
	}

	var separteGraph Graph
	for end := range graph {
		if source == end {
			continue
		}

		newGraph := copyGraph(graph)
		for i := 0; i < minCut; i++ {
			_, cameFrom := breadthFirstSearch(newGraph, source, func(vertice Vertice) bool { return vertice == end })
			path := reconstructPath(source, end, cameFrom)
			for j := 0; j < len(path)-1; j++ {
				edge := Edge{path[j], path[j+1], 1}
				delete(newGraph[path[j]], edge)
			}
		}

		isValid, _ := breadthFirstSearch(newGraph, source, func(vertice Vertice) bool { return vertice == end })
		if !isValid {
			separteGraph = newGraph
			break
		}
	}

	_, cameFrom := breadthFirstSearch(separteGraph, source, func(vertice Vertice) bool { return false })
	lenght1 := len(cameFrom)
	lenght2 := len(separteGraph) - lenght1

	return lenght1 * lenght2
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