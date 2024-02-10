package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
)

type Position struct {
	x, y, risk int
}

type PriorityQueue []Position

func (pq PriorityQueue) Len() int           { return len(pq) }
func (pq PriorityQueue) Less(i, j int) bool { return pq[i].risk < pq[j].risk }
func (pq PriorityQueue) Swap(i, j int)      { pq[i], pq[j] = pq[j], pq[i] }

func (pq *PriorityQueue) Push(x interface{}) {
	*pq = append(*pq, x.(Position))
}

func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	x := old[n-1]
	*pq = old[0 : n-1]
	return x
}

func dijkstra(grid [][]int) int {
	pq := make(PriorityQueue, 0)
	heap.Init(&pq)
	heap.Push(&pq, Position{0, 0, 0})

	rows := len(grid)
	cols := len(grid[0])
	dist := make([][]int, rows)
	for i := range dist {
		dist[i] = make([]int, cols)
		for j := range dist[i] {
			dist[i][j] = 1<<31 - 1 // Set to max int initially
		}
	}
	dist[0][0] = 0

	directions := []Position{{1, 0, 0}, {0, 1, 0}, {-1, 0, 0}, {0, -1, 0}}

	for pq.Len() > 0 {
		curr := heap.Pop(&pq).(Position)
		if curr.x == rows-1 && curr.y == cols-1 {
			return curr.risk
		}
		for _, d := range directions {
			nx, ny := curr.x+d.x, curr.y+d.y
			if nx >= 0 && ny >= 0 && nx < rows && ny < cols {
				nextRisk := curr.risk + grid[nx][ny]
				if nextRisk < dist[nx][ny] {
					dist[nx][ny] = nextRisk
					heap.Push(&pq, Position{nx, ny, nextRisk})
				}
			}
		}
	}
	return -1
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	grid := make([][]int, 0)
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]int, len(line))
		for i, ch := range line {
			row[i] = int(ch - '0')
		}
		grid = append(grid, row)
	}
	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	fmt.Println(dijkstra(grid))
}