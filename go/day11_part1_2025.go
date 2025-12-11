package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {
	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// Parse the graph
	// Map: node -> list of connected nodes
	adj := make(map[string][]string)

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		// Parse line "source: dest1 dest2 ..."
		parts := strings.Split(line, ":")
		if len(parts) != 2 {
			continue // Skip malformed lines
		}

		source := strings.TrimSpace(parts[0])
		destsStr := strings.TrimSpace(parts[1])
		dests := strings.Fields(destsStr) // Split by whitespace

		adj[source] = dests
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	// Count paths from "you" to "out" using DFS with Memoization
	memo := make(map[string]int)
	
	count := countPaths("you", "out", adj, memo)
	
	fmt.Printf("Number of paths from 'you' to 'out': %d\n", count)
}

// countPaths returns the number of paths from current node to target.
// It uses memoization to store results for visited nodes.
func countPaths(current, target string, adj map[string][]string, memo map[string]int) int {
	// Base case: if we reached the target, we found 1 valid path
	if current == target {
		return 1
	}

	// Check if we already computed the count for this node
	if val, ok := memo[current]; ok {
		return val
	}

	totalPaths := 0
	// Iterate over neighbors
	if neighbors, exists := adj[current]; exists {
		for _, neighbor := range neighbors {
			totalPaths += countPaths(neighbor, target, adj, memo)
		}
	}

	// Store result in memo
	memo[current] = totalPaths
	return totalPaths
}
