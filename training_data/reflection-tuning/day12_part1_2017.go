package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	graph := make(map[int][]int)

	// Read input file and build the graph
	file, _ := os.Open("input.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), " <-> ")
		id, _ := strconv.Atoi(parts[0])
		connections := strings.Split(parts[1], ", ")
		for _, conn := range connections {
			connID, _ := strconv.Atoi(conn)
			graph[id] = append(graph[id], connID)
		}
	}

	// Perform DFS starting from program ID 0
	visited := make(map[int]bool)
	count := dfs(graph, 0, visited)

	fmt.Println(count)
}

func dfs(graph map[int][]int, node int, visited map[int]bool) int {
	if visited[node] {
		return 0
	}
	visited[node] = true
	count := 1
	for _, neighbor := range graph[node] {
		count += dfs(graph, neighbor, visited)
	}
	return count
}
