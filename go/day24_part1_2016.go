package main

import (
	"fmt"
	"math"
	"os"
	"path"
	"regexp"
	"runtime"
	"strconv"
	"strings"
)

func main() {
	n := cleaningRobot(readFile("./input.txt"))
	fmt.Println(n)
}

func cleaningRobot(input string) int {
	var grid [][]string
	for _, l := range strings.Split(input, "\n") {
		grid = append(grid, strings.Split(l, ""))
	}

	var graph [][]int
	for r, row := range grid {
		for c, cell := range row {
			if regexp.MustCompile("[0-9]").MatchString(cell) {
				poi := cell
				distancesFromPOI := bfsGetEdgeWeights(grid, [2]int{r, c})

				if graph == nil {
					for i := 0; i < len(distancesFromPOI); i++ {
						graph = append(graph, make([]int, len(distancesFromPOI)))
					}
				}
				index, _ := strconv.Atoi(poi)
				graph[index] = distancesFromPOI
			}
		}
	}

	return dfs(graph, 0, map[int]bool{0: true}, false)
}

type bfsNode struct {
	row, col int
	distance int
}

func bfsGetEdgeWeights(grid [][]string, start [2]int) []int {

	poiToDistance := map[string]int{
		grid[start[0]][start[1]]: 0,
	}

	queue := []bfsNode{
		{start[0], start[1], 0},
	}
	visited := map[[2]int]bool{}
	for len(queue) > 0 {
		front := queue[0]
		queue = queue[1:]

		if visited[[2]int{front.row, front.col}] {
			continue
		}
		visited[[2]int{front.row, front.col}] = true

		if regexp.MustCompile("[0-9]").MatchString(grid[front.row][front.col]) {
			poiToDistance[grid[front.row][front.col]] = front.distance
		}
		for _, d := range dirs {
			nextRow, nextCol := front.row+d[0], front.col+d[1]

			if grid[nextRow][nextCol] != "#" {
				queue = append(queue, bfsNode{
					row:      nextRow,
					col:      nextCol,
					distance: front.distance + 1,
				})
			}
		}

	}

	distances := make([]int, len(poiToDistance))
	for numStr, dist := range poiToDistance {
		n, _ := strconv.Atoi(numStr)
		distances[n] = dist
	}
	return distances
}

var dirs = [][2]int{
	{0, -1},
	{0, 1},
	{1, 0},
	{-1, 0},
}

func dfs(graph [][]int, entryIndex int, visited map[int]bool, returnToZero bool) (minWeightSum int) {

	if len(graph) == len(visited) {
		if returnToZero {
			return graph[entryIndex][0]
		}
		return 0
	}

	minDistance := math.MaxInt32
	for i, val := range graph[entryIndex] {
		if !visited[i] {
			visited[i] = true

			dist := val + dfs(graph, i, visited, returnToZero)
			minDistance = minInt(minDistance, dist)

			delete(visited, i)
		}
	}

	return minDistance
}

func readFile(pathFromCaller string) string {
	_, filename, _, ok := runtime.Caller(1)
	if !ok {
		panic("Not Found")
	}

	absolutePath := path.Join(path.Dir(filename), pathFromCaller)

	content, err := os.ReadFile(absolutePath)
	if err != nil {
		panic(err)
	}

	strContent := string(content)
	return strings.TrimRight(strContent, "\n")
}

func minInt(a, b int) int {
	if a < b {
		return a
	}
	return b
}