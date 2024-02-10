package main

import (
	"fmt"
	"os"
	"strings"
)

type MinHeap struct {
	heap
}

func NewMinHeap() *MinHeap {
	nestedHeap := heap{
		closerToRoot: func(val1, val2 int) bool {
			return val1 < val2
		},
	}
	return &MinHeap{nestedHeap}
}

type MaxHeap struct {
	heap
}

func NewMaxHeap() *MaxHeap {
	nestedHeap := heap{
		closerToRoot: func(val1, val2 int) bool {
			return val1 > val2
		},
	}
	return &MaxHeap{nestedHeap}
}

type heap struct {
	nodes        []HeapNode
	closerToRoot func(val1, val2 int) bool
}

type HeapNode interface {
	Value() int
}

func (h *heap) Front() HeapNode {
	if len(h.nodes) == 0 {
		return nil
	}
	return h.nodes[0]
}

func (h *heap) Add(newNode HeapNode) {
	h.nodes = append(h.nodes, newNode)
	h.heapifyFromEnd()
}

func (h *heap) Remove() HeapNode {
	if len(h.nodes) == 0 {
		return nil
	}

	rootNode := h.nodes[0]

	h.nodes[0] = h.nodes[len(h.nodes)-1]
	h.nodes = h.nodes[:len(h.nodes)-1]

	h.heapifyFromStart()

	return rootNode
}

func (h *heap) Length() int {
	return len(h.nodes)
}

func (h *heap) swap(i, j int) {
	h.nodes[i], h.nodes[j] = h.nodes[j], h.nodes[i]
}

func (h *heap) heapifyFromEnd() {
	currentIndex := len(h.nodes) - 1
	for currentIndex > 0 {
		parentIndex := (currentIndex - 1) / 2
		parentNode := h.nodes[parentIndex]
		if h.closerToRoot(h.nodes[currentIndex].Value(), parentNode.Value()) {
			h.swap(parentIndex, currentIndex)
			currentIndex = parentIndex
		} else {
			break
		}
	}
}

func (h *heap) heapifyFromStart() {
	currentIndex := 0

	for {

		smallerChildIndex := currentIndex
		for i := 1; i <= 2; i++ {
			childIndex := currentIndex*2 + i

			if childIndex < len(h.nodes) &&
				h.closerToRoot(h.nodes[childIndex].Value(), h.nodes[smallerChildIndex].Value()) {
				smallerChildIndex = childIndex
			}
		}

		if smallerChildIndex == currentIndex {
			return
		}

		h.swap(smallerChildIndex, currentIndex)
		currentIndex = smallerChildIndex
	}
}

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(file))

	ans := amphipod(input)
	fmt.Println(ans)
}

var roomCoordToWantChar = map[[2]int]string{
	{2, 3}: "A", {3, 3}: "A", {4, 3}: "A", {5, 3}: "A",
	{2, 5}: "B", {3, 5}: "B", {4, 5}: "B", {5, 5}: "B",
	{2, 7}: "C", {3, 7}: "C", {4, 7}: "C", {5, 7}: "C",
	{2, 9}: "D", {3, 9}: "D", {4, 9}: "D", {5, 9}: "D",
}

func amphipod(input string) int {
	start := parseInput(input)

	roomCoordToWantChar := roomCoordToWantChar

	start.grid = append(start.grid, nil, nil)
	start.grid[6] = start.grid[4]
	start.grid[5] = start.grid[3]

	start.grid[3] = strings.Split("  #D#C#B#A#  ", "")
	start.grid[4] = strings.Split("  #D#B#A#C#  ", "")

	minHeap := NewMinHeap()

	minHeap.Add(start)
	seenGrids := map[string]bool{}
	for minHeap.Length() > 0 {
		front := minHeap.Remove().(*state)

		key := fmt.Sprint(front.grid)
		if seenGrids[key] {
			continue
		}
		seenGrids[key] = true

		if front.allDone(roomCoordToWantChar) {
			return front.energyUsed
		}

		unsettledCoords := front.getUnsettledCoords(roomCoordToWantChar)
		for _, unsettledCoord := range unsettledCoords {
			ur, uc := unsettledCoord[0], unsettledCoord[1]
			nextMoves := front.getNextPossibleMoves(unsettledCoord, roomCoordToWantChar)
			for _, nextCoord := range nextMoves {
				nr, nc := nextCoord[0], nextCoord[1]
				if front.grid[nr][nc] != "." {
					panic(fmt.Sprintf("should only be moving to walkable spaces, got %q at %d,%d", front.grid[nr][nc], nr, nc))
				}

				cp := front.copy()

				cp.energyUsed += calcEnergy(cp.grid[ur][uc], unsettledCoord, nextCoord)
				cp.path += fmt.Sprintf("%s%v->%v{%d},", front.grid[ur][uc], unsettledCoord, nextCoord, cp.energyUsed)
				cp.grid[nr][nc], cp.grid[ur][uc] = cp.grid[ur][uc], cp.grid[nr][nc]

				minHeap.Add(cp)
			}
		}
	}

	panic("should return from loop")
}

type state struct {
	grid       [][]string
	energyUsed int
	path       string
}

func parseInput(input string) *state {
	grid := [][]string{}
	for _, line := range strings.Split(input, "\n") {
		grid = append(grid, strings.Split(line, ""))
	}
	return &state{
		grid: grid,
	}
}

func (s *state) Value() int {
	return s.energyUsed
}

func (s *state) String() string {
	var sb strings.Builder
	for _, row := range s.grid {
		for _, unsettledChar := range row {
			sb.WriteString(unsettledChar)
		}
		sb.WriteRune('\n')
	}

	sb.WriteString(fmt.Sprintf("nrg: %d, ,path: %s\n", s.energyUsed, s.path))

	return sb.String()
}

func (s *state) copy() *state {
	cp := state{
		grid:       make([][]string, len(s.grid)),
		energyUsed: s.energyUsed,
		path:       s.path,
	}

	for i := range cp.grid {
		cp.grid[i] = make([]string, len(s.grid[i]))
		copy(cp.grid[i], s.grid[i])
	}

	return &cp
}

func (s *state) allDone(roomCoordToWantChar map[[2]int]string) bool {
	for coord, want := range roomCoordToWantChar {
		if s.grid[coord[0]][coord[1]] != want {
			return false
		}
	}
	return true
}

func (s *state) getUnsettledCoords(roomCoordToWantChar map[[2]int]string) [][2]int {
	var unsettled [][2]int

	for col := 1; col < len(s.grid[0]); col++ {
		if strings.Contains("ABCD", s.grid[1][col]) {
			unsettled = append(unsettled, [2]int{1, col})
		}
	}

	for _, col := range []int{3, 5, 7, 9} {
		roomFullFromBack := true
		for row := len(s.grid) - 2; row >= 2; row-- {
			coord := [2]int{row, col}
			wantChar := roomCoordToWantChar[coord]
			gotChar := s.grid[row][col]
			if gotChar != "." {
				if gotChar != wantChar {
					roomFullFromBack = false
					unsettled = append(unsettled, coord)
				} else if gotChar == wantChar && !roomFullFromBack {

					unsettled = append(unsettled, coord)
				}
			}
		}
	}
	return unsettled
}

var coordsInFrontOfRooms = map[[2]int]bool{
	{1, 3}: true,
	{1, 5}: true,
	{1, 7}: true,
	{1, 9}: true,
}

func isInHallway(coord [2]int) bool {
	return coord[0] == 1
}

func (s *state) getNextPossibleMoves(unsettledCoord [2]int, roomCoordToWantChar map[[2]int]string) [][2]int {

	unsettledChar := s.grid[unsettledCoord[0]][unsettledCoord[1]]

	if !strings.Contains("ABCD", unsettledChar) {
		panic("unexpected character to get next moves for " + unsettledChar)
	}

	var possible [][2]int

	startedInHallway := isInHallway(unsettledCoord)

	queue := [][2]int{unsettledCoord}
	seen := map[[2]int]bool{}
	for len(queue) > 0 {
		front := queue[0]
		queue = queue[1:]

		if seen[front] {
			continue
		}
		seen[front] = true

		if front != unsettledCoord {

			if !coordsInFrontOfRooms[front] {
				wantChar, isRoomCoord := roomCoordToWantChar[front]

				if !isRoomCoord {

					if !startedInHallway {
						possible = append(possible, front)
					}
				} else if wantChar == unsettledChar {
					isStuckAmphipod := false
					roomHasDeeperOpenSpaces := false
					for r := front[0] + 1; r < len(s.grid)-1; r++ {
						char := s.grid[r][front[1]]
						if char == "." {
							roomHasDeeperOpenSpaces = true
						}
						if char != "." && char != unsettledChar {
							isStuckAmphipod = true
							break
						}
					}

					if !roomHasDeeperOpenSpaces && !isStuckAmphipod {
						possible = append(possible, front)
					}
				}
			}
		}

		for _, d := range [][2]int{

			{-1, 0},
			{1, 0},
			{0, -1},
			{0, 1},
		} {

			next := [2]int{front[0] + d[0], front[1] + d[1]}
			if s.grid[next[0]][next[1]] == "." {

				queue = append(queue, next)
			}
		}
	}

	return possible
}

func calcEnergy(char string, start, end [2]int) int {

	dist := absInt(end[1] - start[1])

	dist += start[0] - 1
	dist += end[0] - 1

	energyPerType := map[string]int{
		"A": 1,
		"B": 10,
		"C": 100,
		"D": 1000,
	}

	if _, ok := energyPerType[char]; !ok {
		panic(char + " should not call calcEnergy()")
	}
	return energyPerType[char] * dist
}

func absInt(n int) int {
	if n < 0 {
		return -n
	}
	return n
}