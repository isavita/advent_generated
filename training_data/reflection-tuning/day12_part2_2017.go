package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	graph := parseInput("input.txt")

	// Part 1
	visited := make(map[int]bool)
	dfs(0, graph, visited)
	fmt.Println("Part 1:", len(visited))

	// Part 2
	visited = make(map[int]bool)
	groups := 0
	for program := range graph {
		if !visited[program] {
			dfs(program, graph, visited)
			groups++
		}
	}
	fmt.Println("Part 2:", groups)
}

func parseInput(filename string) map[int][]int {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	graph := make(map[int][]int)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), " <-> ")
		program, _ := strconv.Atoi(parts[0])
		connections := strings.Split(parts[1], ", ")
		for _, conn := range connections {
			connID, _ := strconv.Atoi(conn)
			graph[program] = append(graph[program], connID)
		}
	}
	return graph
}

func dfs(program int, graph map[int][]int, visited map[int]bool) {
	if visited[program] {
		return
	}
	visited[program] = true
	for _, neighbor := range graph[program] {
		dfs(neighbor, graph, visited)
	}
}
