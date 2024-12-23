package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

var graph map[string]map[string]bool
var bestClique []string

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	graph = make(map[string]map[string]bool)
	scanner := bufio.NewScanner(file)
	nodesSet := make(map[string]bool)

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, "-")
		if len(parts) != 2 {
			continue
		}
		a, b := parts[0], parts[1]
		if graph[a] == nil {
			graph[a] = make(map[string]bool)
		}
		if graph[b] == nil {
			graph[b] = make(map[string]bool)
		}
		graph[a][b] = true
		graph[b][a] = true
		nodesSet[a] = true
		nodesSet[b] = true
	}
	if err := scanner.Err(); err != nil {
		panic(err)
	}

	var allNodes []string
	for n := range nodesSet {
		allNodes = append(allNodes, n)
	}

	BronKerbosch(nil, allNodes, nil)
	sort.Strings(bestClique)
	fmt.Println(strings.Join(bestClique, ","))
}

func BronKerbosch(R, P, X []string) {
	if len(P) == 0 && len(X) == 0 {
		if len(R) > len(bestClique) {
			bestClique = append([]string(nil), R...)
		}
		return
	}
	// Copy of P to avoid modifying caller's slice
	tempP := append([]string(nil), P...)
	for _, v := range tempP {
		neighbors := neighborsOf(v)
		BronKerbosch(
			union(R, v),
			intersect(P, neighbors),
			intersect(X, neighbors),
		)
		P = remove(P, v)
		X = union(X, v)
	}
}

func neighborsOf(node string) map[string]bool {
	return graph[node]
}

func intersect(a []string, b map[string]bool) []string {
	var out []string
	for _, x := range a {
		if b[x] {
			out = append(out, x)
		}
	}
	return out
}

func union(a []string, x string) []string {
	out := append([]string(nil), a...)
	out = append(out, x)
	return out
}

func remove(slice []string, s string) []string {
	var out []string
	for _, x := range slice {
		if x != s {
			out = append(out, x)
		}
	}
	return out
}
