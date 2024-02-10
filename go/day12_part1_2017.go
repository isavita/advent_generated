package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// DFS function to traverse the connections
func DFS(node int, adj map[int][]int, visited map[int]bool) {
	visited[node] = true
	for _, neighbor := range adj[node] {
		if !visited[neighbor] {
			DFS(neighbor, adj, visited)
		}
	}
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}
	defer file.Close()

	// Adjacency list to store connections
	adj := make(map[int][]int)
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " <-> ")
		from, _ := strconv.Atoi(parts[0])
		toNodes := strings.Split(parts[1], ", ")

		for _, toNode := range toNodes {
			to, _ := strconv.Atoi(toNode)
			adj[from] = append(adj[from], to)
			adj[to] = append(adj[to], from)
		}
	}

	// Set to keep track of visited nodes
	visited := make(map[int]bool)

	// Start DFS from node 0
	DFS(0, adj, visited)

	// Count the number of visited nodes
	count := 0
	for _, v := range visited {
		if v {
			count++
		}
	}

	fmt.Println(count)
}