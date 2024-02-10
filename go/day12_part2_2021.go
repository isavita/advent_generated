package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(file))
	ans := solve(input)
	fmt.Println(ans)

}

func solve(input string) int {
	parsed := parseInput(input)

	graph := map[string]map[string]bool{}
	for _, pair := range parsed {
		if graph[pair[0]] == nil {
			graph[pair[0]] = map[string]bool{}
		}
		if graph[pair[1]] == nil {
			graph[pair[1]] = map[string]bool{}
		}
		graph[pair[0]][pair[1]] = true
		graph[pair[1]][pair[0]] = true
	}

	return walk(graph, "start", map[string]int{"start": 5}, []string{"start"}, false)
}

func walk(graph map[string]map[string]bool, current string, visited map[string]int, path []string, doubleUsed bool) int {
	if current == "end" {
		return 1
	}

	visited[current]++

	var pathsToEnd int

	for visitable := range graph[current] {
		if visitable == "start" {
			continue
		}

		if strings.ToUpper(visitable) != visitable && visited[visitable] > 0 {
			if doubleUsed {
				continue
			} else {
				doubleUsed = true
			}
		}

		path = append(path, visitable)
		pathsToEnd += walk(graph, visitable, visited, path, doubleUsed)

		visited[visitable]--
		path = path[:len(path)-1]

		if strings.ToUpper(visitable) != visitable && visited[visitable] == 1 {
			doubleUsed = false
		}
	}

	return pathsToEnd
}

func parseInput(input string) (ans [][]string) {
	for _, line := range strings.Split(input, "\n") {
		ans = append(ans, strings.Split(line, "-"))
	}
	return ans
}