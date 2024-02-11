package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type Cave struct {
	connections map[string]bool
}

func NewCave() *Cave {
	return &Cave{
		connections: make(map[string]bool),
	}
}

func (c *Cave) ConnectTo(name string) {
	c.connections[name] = true
}

func (c *Cave) DisconnectFrom(name string) {
	delete(c.connections, name)
}

func main() {
	caves := make(map[string]*Cave)
	file, _ := os.Open("input.txt")
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		paths := strings.Split(line, "-")
		from, to := paths[0], paths[1]

		if _, ok := caves[from]; !ok {
			caves[from] = NewCave()
		}

		if _, ok := caves[to]; !ok {
			caves[to] = NewCave()
		}

		caves[from].ConnectTo(to)
		caves[to].ConnectTo(from)
	}

	count := 0
	var dfs func(string, map[string]bool)
	dfs = func(current string, visited map[string]bool) {
		if current == "end" {
			count++
			return
		}

		for next := range caves[current].connections {
			if visited[next] && strings.ToLower(next) == next {
				continue
			}

			visitedCopy := make(map[string]bool)
			for k, v := range visited {
				visitedCopy[k] = v
			}
			visitedCopy[next] = true
			dfs(next, visitedCopy)
		}
	}

	dfs("start", map[string]bool{"start": true})
	fmt.Println(count)
}
