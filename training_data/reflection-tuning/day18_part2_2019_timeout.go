package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
	"strings"
)

type Point struct {
	x, y int
}

type State struct {
	positions [4]Point
	keys      uint32
	steps     int
}

type PQItem struct {
	state    State
	priority int
	index    int
}

type PriorityQueue []*PQItem

func (pq PriorityQueue) Len() int           { return len(pq) }
func (pq PriorityQueue) Less(i, j int) bool { return pq[i].priority < pq[j].priority }
func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}
func (pq *PriorityQueue) Push(x interface{}) {
	n := len(*pq)
	item := x.(*PQItem)
	item.index = n
	*pq = append(*pq, item)
}
func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = nil
	item.index = -1
	*pq = old[0 : n-1]
	return item
}

var grid []string
var keyPositions map[rune]Point
var startPositions []Point
var allKeys uint32

func main() {
	readInput()
	fmt.Println("Part 1:", solve(1))
	updateGridForPart2()
	fmt.Println("Part 2:", solve(4))
}

func readInput() {
	file, _ := os.Open("input.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}
}

func updateGridForPart2() {
	for i, row := range grid {
		if strings.Contains(row, "@") {
			above := []rune(grid[i-1])
			current := []rune(row)
			below := []rune(grid[i+1])
			x := strings.IndexRune(row, '@')
			above[x-1], above[x], above[x+1] = '@', '#', '@'
			current[x-1], current[x], current[x+1] = '#', '#', '#'
			below[x-1], below[x], below[x+1] = '@', '#', '@'
			grid[i-1] = string(above)
			grid[i] = string(current)
			grid[i+1] = string(below)
			break
		}
	}
}

func solve(robotCount int) int {
	keyPositions = make(map[rune]Point)
	startPositions = nil
	allKeys = 0

	for y, row := range grid {
		for x, ch := range row {
			if ch == '@' {
				startPositions = append(startPositions, Point{x, y})
			} else if ch >= 'a' && ch <= 'z' {
				keyPositions[ch] = Point{x, y}
				allKeys |= 1 << (ch - 'a')
			}
		}
	}

	for len(startPositions) < robotCount {
		startPositions = append(startPositions, startPositions[0])
	}

	initialState := State{steps: 0, keys: 0}
	for i, pos := range startPositions[:robotCount] {
		initialState.positions[i] = pos
	}

	pq := make(PriorityQueue, 0)
	heap.Init(&pq)
	heap.Push(&pq, &PQItem{state: initialState, priority: 0})

	visited := make(map[string]bool)

	for pq.Len() > 0 {
		item := heap.Pop(&pq).(*PQItem)
		state := item.state

		if state.keys == allKeys {
			return state.steps
		}

		stateKey := fmt.Sprintf("%v|%d", state.positions, state.keys)
		if visited[stateKey] {
			continue
		}
		visited[stateKey] = true

		for robot := 0; robot < robotCount; robot++ {
			for key, keyPos := range keyPositions {
				if state.keys&(1<<(key-'a')) != 0 {
					continue
				}
				steps := bfs(state.positions[robot], keyPos, state.keys)
				if steps == -1 {
					continue
				}
				newState := state
				newState.positions[robot] = keyPos
				newState.keys |= 1 << (key - 'a')
				newState.steps += steps
				priority := newState.steps + countBits(allKeys^newState.keys)
				heap.Push(&pq, &PQItem{state: newState, priority: priority})
			}
		}
	}

	return -1
}

func bfs(start, end Point, keys uint32) int {
	queue := []Point{start}
	visited := make(map[Point]bool)
	distances := make(map[Point]int)
	visited[start] = true
	distances[start] = 0

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if current == end {
			return distances[current]
		}

		for _, dir := range []Point{{0, 1}, {1, 0}, {0, -1}, {-1, 0}} {
			next := Point{current.x + dir.x, current.y + dir.y}
			if next.y < 0 || next.y >= len(grid) || next.x < 0 || next.x >= len(grid[0]) {
				continue
			}
			ch := rune(grid[next.y][next.x])
			if ch == '#' || visited[next] {
				continue
			}
			if ch >= 'A' && ch <= 'Z' && keys&(1<<(ch-'A')) == 0 {
				continue
			}
			visited[next] = true
			distances[next] = distances[current] + 1
			queue = append(queue, next)
		}
	}

	return -1
}

func countBits(n uint32) int {
	count := 0
	for n != 0 {
		count++
		n &= n - 1
	}
	return count
}
