package main

import (
	"container/heap"
	"fmt"
	"os"
	"strings"
)

type IntPriorityQueueItem struct {
	Item     interface{}
	Priority int
}

type IntPriorityQueue []IntPriorityQueueItem

func (q IntPriorityQueue) Len() int {
	return len(q)
}

func (q IntPriorityQueue) Less(i, j int) bool {
	return q[i].Priority < q[j].Priority
}

func (q IntPriorityQueue) Swap(i, j int) {
	q[i], q[j] = q[j], q[i]
}

func (q *IntPriorityQueue) Push(x interface{}) {
	item := x.(IntPriorityQueueItem)
	*q = append(*q, item)
}

func (q *IntPriorityQueue) Pop() interface{} {
	old := *q
	n := len(old)
	item := old[n-1]
	*q = old[0 : n-1]
	return item
}

type Coord struct {
	X int
	Y int
}

func (c1 Coord) Add(c2 Coord) Coord {
	return Coord{c1.X + c2.X, c1.Y + c2.Y}
}

func (c1 Coord) Substract(c2 Coord) Coord {
	return Coord{c1.X - c2.X, c1.Y - c2.Y}
}

func (c1 Coord) opposite() Coord {
	return Coord{-c1.X, -c1.Y}
}

type Grid struct {
	Width  int
	Height int
	Data   map[Coord]int
}

var (
	North = Coord{0, -1}
	West  = Coord{-1, 0}
	South = Coord{0, 1}
	East  = Coord{1, 0}
)

func (coord Coord) isInBounds(grid Grid) bool {
	return 0 <= coord.X && coord.X < grid.Width && 0 <= coord.Y && coord.Y < grid.Height
}

func Abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func buildGrid(input []string) Grid {
	grid := Grid{
		Width:  len(input[0]),
		Height: len(input),
		Data:   make(map[Coord]int, len(input)*len(input[0])),
	}

	for y, line := range input {
		for x, char := range line {
			grid.Data[Coord{x, y}] = int(char - '0')
		}
	}

	return grid
}

func (grid Grid) neighbors4(coord Coord) []Coord {
	neighbors := []Coord{}
	directions := []Coord{North, West, South, East}

	for _, dir := range directions {
		neighbor := coord.Add(dir)
		if neighbor.isInBounds(grid) {
			neighbors = append(neighbors, neighbor)
		}
	}

	return neighbors
}

func (grid Grid) dijkstra(start Coord, goal Coord) (map[Coord]Coord, map[Coord]int) {
	frontier := &IntPriorityQueue{}
	heap.Init(frontier)
	heap.Push(frontier, IntPriorityQueueItem{Item: start, Priority: 0})

	cameFrom := make(map[Coord]Coord)
	costSoFar := make(map[Coord]int)
	cameFrom[start] = start
	costSoFar[start] = 0

	for frontier.Len() > 0 {
		minItem := heap.Pop(frontier).(IntPriorityQueueItem)
		current := minItem.Item.(Coord)
		currentCost := minItem.Priority

		if current == goal {
			break
		}

		for _, next := range grid.neighbors4(current) {
			newCost := currentCost + grid.Data[next]
			if cost, isFound := costSoFar[next]; !isFound || newCost < cost {
				costSoFar[next] = newCost
				priority := newCost
				heap.Push(frontier, IntPriorityQueueItem{Item: next, Priority: priority})
				cameFrom[next] = current
			}
		}
	}

	return cameFrom, costSoFar
}

func heuristic(c1 Coord, c2 Coord) int {
	return Abs(c1.X-c2.X) + Abs(c1.Y-c2.Y)
}

func (grid Grid) AStarConstrained(start Coord, goal Coord, minStraight int, maxStraight int) int {
	type Info struct {
		coord       Coord
		dir         Coord
		numStraight int
	}
	startInfo := Info{coord: start, dir: Coord{}, numStraight: 0}

	frontier := &IntPriorityQueue{}
	heap.Init(frontier)
	heap.Push(frontier, IntPriorityQueueItem{Item: startInfo, Priority: 0})

	cameFrom := make(map[Info]Info)
	costSoFar := make(map[Info]int)
	cameFrom[startInfo] = startInfo
	costSoFar[startInfo] = 0

	for frontier.Len() > 0 {
		minItem := heap.Pop(frontier).(IntPriorityQueueItem)
		current := minItem.Item.(Info)
		currentCost := costSoFar[current]

		if current.coord == goal {
			return currentCost
		}

		for _, next := range grid.neighbors4(current.coord) {
			newDir := next.Substract(current.coord)
			newNumStraight := 1
			if newDir == current.dir {
				newNumStraight += current.numStraight
			}

			nextInfo := Info{
				coord:       next,
				dir:         newDir,
				numStraight: newNumStraight,
			}
			newCost := currentCost + grid.Data[next]
			actualCost, isFound := costSoFar[nextInfo]

			isLowerCost := !isFound || newCost < actualCost
			isValidStraight := (current.numStraight >= minStraight || newDir == current.dir || current.coord == start) &&
				(newNumStraight <= maxStraight)
			isNotOppositeDirection := newDir != current.dir.opposite()

			isValid := isLowerCost && isValidStraight && isNotOppositeDirection
			if isValid {
				costSoFar[nextInfo] = newCost
				cameFrom[nextInfo] = current

				priority := newCost + heuristic(next, goal)
				queueItem := IntPriorityQueueItem{Item: nextInfo, Priority: priority}
				heap.Push(frontier, queueItem)
			}
		}
	}

	return -1
}

func solve(input []string) int {
	grid := buildGrid(input)

	start := Coord{0, 0}
	goal := Coord{grid.Width - 1, grid.Height - 1}
	res := grid.AStarConstrained(start, goal, 4, 10)

	return res
}

func readFile(fileName string) []string {
	file, err := os.ReadFile(fileName)
	if err != nil {
		panic(err)
	}

	return strings.Split(strings.TrimSpace(string(file)), "\n")
}

func main() {
	input := readFile("input.txt")
	fmt.Println(solve(input))
}