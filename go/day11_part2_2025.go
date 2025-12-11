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
	adj := make(map[string][]string)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Split(line, ":")
		if len(parts) != 2 {
			continue
		}
		source := strings.TrimSpace(parts[0])
		destsStr := strings.TrimSpace(parts[1])
		dests := strings.Fields(destsStr)
		adj[source] = dests
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	// We need paths from 'svr' to 'out' passing through 'dac' and 'fft'.
	// In a DAG, the order must be either dac->fft or fft->dac.
	// We calculate both and sum them (one will likely be 0).

	// Scenario 1: svr -> dac -> fft -> out
	// Count = (svr->dac) * (dac->fft) * (fft->out)
	s1_part1 := countPaths("svr", "dac", adj)
	s1_part2 := countPaths("dac", "fft", adj)
	s1_part3 := countPaths("fft", "out", adj)
	totalScenario1 := s1_part1 * s1_part2 * s1_part3

	// Scenario 2: svr -> fft -> dac -> out
	// Count = (svr->fft) * (fft->dac) * (dac->out)
	s2_part1 := countPaths("svr", "fft", adj)
	s2_part2 := countPaths("fft", "dac", adj)
	s2_part3 := countPaths("dac", "out", adj)
	totalScenario2 := s2_part1 * s2_part2 * s2_part3

	fmt.Printf("Paths (svr->dac->fft->out): %d\n", totalScenario1)
	fmt.Printf("Paths (svr->fft->dac->out): %d\n", totalScenario2)
	fmt.Printf("Total paths visiting both: %d\n", totalScenario1+totalScenario2)
}

// countPaths calculates the number of distinct paths from start to end using DFS + Memoization
func countPaths(start, end string, adj map[string][]string) int {
	memo := make(map[string]int)
	return dfs(start, end, adj, memo)
}

func dfs(current, target string, adj map[string][]string, memo map[string]int) int {
	if current == target {
		return 1
	}
	if count, ok := memo[current]; ok {
		return count
	}

	total := 0
	if neighbors, ok := adj[current]; ok {
		for _, next := range neighbors {
			total += dfs(next, target, adj, memo)
		}
	}

	memo[current] = total
	return total
}
