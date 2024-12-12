package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Move struct {
	label string
	x     int
	y     int
}

func main() {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}

	args := strings.Split(string(input), "\n")
	var graph [][]string

	for _, line := range args {
		if line == "" {
			continue
		}
		row := strings.Split(line, "")
		graph = append(graph, row)
	}

	H := len(graph)
	W := len(graph[0])

	move := []Move{
		{label: "left", x: -1, y: 0},
		{label: "up", x: 0, y: -1},
		{label: "right", x: 1, y: 0},
		{label: "down", x: 0, y: 1},
	}

	sum := 0

	for y := 0; y < H; y++ {
		for x := 0; x < W; x++ {
			if graph[y][x] == "." {
				continue
			}

			area := 0
			target := graph[y][x]
			visited := make(map[string]bool)
			side := make(map[string]map[string]bool)

			var search func(cx, cy int, label string)
			search = func(cx, cy int, label string) {
				if graph[cy][cx] != target {
					if label != "" && !visited[fmt.Sprintf("%d,%d", cx, cy)] {
						saveOuter(label, side, cx, cy)
					}
					return
				}

				visited[fmt.Sprintf("%d,%d", cx, cy)] = true
				area++
				graph[cy][cx] = "."

				for _, m := range move {
					nx := cx + m.x
					ny := cy + m.y

					if nx < 0 || nx >= W || ny < 0 || ny >= H {
						saveOuter(m.label, side, nx, ny)
						continue
					}
					search(nx, ny, m.label)
				}
			}

			search(x, y, "")
			outer := countOuter(side)
			sum += area * outer
		}
	}
	fmt.Println(sum)
}

func saveOuter(label string, side map[string]map[string]bool, x, y int) {
	var key string
	if label == "up" || label == "down" {
		key = fmt.Sprintf("%d:%d", y, x)
	} else {
		key = fmt.Sprintf("%d:%d", x, y)
	}

	if side[label] == nil {
		side[label] = make(map[string]bool)
	}
	side[label][key] = true
}

func countOuter(side map[string]map[string]bool) int {
	outer := 0
	for label := range side {
		var array []string
		for key := range side[label] {
			array = append(array, key)
		}

		sort.Slice(array, func(i, j int) bool {
			iParts := strings.Split(array[i], ":")
			jParts := strings.Split(array[j], ":")
			iFirst, _ := strconv.Atoi(iParts[0])
			jFirst, _ := strconv.Atoi(jParts[0])
			if iFirst == jFirst {
				iSecond, _ := strconv.Atoi(iParts[1])
				jSecond, _ := strconv.Atoi(jParts[1])
				return iSecond < jSecond
			}
			return iFirst < jFirst
		})

		var temp []string
		for _, current := range array {
			parts := strings.Split(current, ":")
			i, _ := strconv.Atoi(parts[0])
			j, _ := strconv.Atoi(parts[1])
			if !check(temp, i, j) {
				outer++
			}
			temp = append(temp, current)
		}
	}
	return outer
}

func check(ary []string, i, j int) bool {
	search := []string{
		fmt.Sprintf("%d:%d", i, j-1),
		fmt.Sprintf("%d:%d", i, j+1),
	}
	for _, s := range search {
		for _, a := range ary {
			if a == s {
				return true
			}
		}
	}
	return false
}
