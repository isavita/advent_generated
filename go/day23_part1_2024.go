package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// Graph represents the network connections using an adjacency list
type Graph map[string]map[string]bool

func main() {
	// Read input file
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	// Initialize graph
	graph := make(Graph)

	// Read connections and build graph
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		computers := strings.Split(line, "-")
		if len(computers) != 2 {
			continue
		}

		// Add both directions since connections are bidirectional
		addConnection(graph, computers[0], computers[1])
		addConnection(graph, computers[1], computers[0])
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	// Find all sets of three inter-connected computers
	tripletsWithT := findTripletsWithT(graph)

	fmt.Println("Number of triplets containing at least one computer with name starting with 't':", len(tripletsWithT))
}

// addConnection adds a connection between two computers in the graph
func addConnection(graph Graph, from, to string) {
	if _, exists := graph[from]; !exists {
		graph[from] = make(map[string]bool)
	}
	graph[from][to] = true
}

// findTripletsWithT finds all sets of three inter-connected computers where at least one name starts with 't'
func findTripletsWithT(graph Graph) [][]string {
	var triplets [][]string
	seen := make(map[string]bool)

	// Get all computers
	computers := make([]string, 0, len(graph))
	for computer := range graph {
		computers = append(computers, computer)
	}

	// Check all possible combinations of three computers
	for i := 0; i < len(computers); i++ {
		for j := i + 1; j < len(computers); j++ {
			for k := j + 1; k < len(computers); k++ {
				c1, c2, c3 := computers[i], computers[j], computers[k]

				// Check if they form a triplet (all connected to each other)
				if graph[c1][c2] && graph[c2][c3] && graph[c1][c3] {
					// Check if at least one computer starts with 't'
					if strings.HasPrefix(c1, "t") || strings.HasPrefix(c2, "t") || strings.HasPrefix(c3, "t") {
						// Create a unique key for this triplet to avoid duplicates
						triplet := []string{c1, c2, c3}
						key := createTripletKey(triplet)
						if !seen[key] {
							triplets = append(triplets, triplet)
							seen[key] = true
						}
					}
				}
			}
		}
	}

	return triplets
}

// createTripletKey creates a unique key for a triplet by sorting and joining the computer names
func createTripletKey(triplet []string) string {
	// Sort the triplet to ensure consistent keys
	sorted := make([]string, len(triplet))
	copy(sorted, triplet)
	for i := 0; i < len(sorted)-1; i++ {
		for j := i + 1; j < len(sorted); j++ {
			if sorted[i] > sorted[j] {
				sorted[i], sorted[j] = sorted[j], sorted[i]
			}
		}
	}
	return strings.Join(sorted, ",")
}
