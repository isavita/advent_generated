package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type State struct {
	elevator int
	floors   [4]uint16
}

func (s State) isValid() bool {
	for _, floor := range s.floors {
		generators := floor >> 8
		microchips := floor & 0xFF
		if microchips&^generators != 0 && generators != 0 {
			return false
		}
	}
	return true
}

func (s State) isGoal() bool {
	return s.floors[3] == 0xFFFF && s.floors[0] == 0 && s.floors[1] == 0 && s.floors[2] == 0
}

func (s State) hash() uint64 {
	h := uint64(s.elevator)
	for i, floor := range s.floors {
		h |= uint64(floor) << (uint(i)*16 + 2)
	}
	return h
}

func bfs(initial State) int {
	queue := []State{initial}
	seen := make(map[uint64]bool)
	seen[initial.hash()] = true
	steps := 0

	for len(queue) > 0 {
		levelSize := len(queue)
		for i := 0; i < levelSize; i++ {
			current := queue[0]
			queue = queue[1:]

			if current.isGoal() {
				return steps
			}

			for nextFloor := 0; nextFloor < 4; nextFloor++ {
				if nextFloor == current.elevator || abs(nextFloor-current.elevator) > 1 {
					continue
				}

				currentFloor := current.floors[current.elevator]
				for item1 := uint16(1); item1 <= 0x8000; item1 <<= 1 {
					if currentFloor&item1 == 0 {
						continue
					}
					for item2 := item1; item2 <= 0x8000; item2 <<= 1 {
						if item1 != item2 && currentFloor&item2 == 0 {
							continue
						}
						next := current
						next.elevator = nextFloor
						next.floors[current.elevator] &^= item1 | item2
						next.floors[nextFloor] |= item1 | item2

						if next.isValid() {
							h := next.hash()
							if !seen[h] {
								seen[h] = true
								queue = append(queue, next)
							}
						}
					}
				}
			}
		}
		steps++
	}
	return -1
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func parseInput(filename string) State {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)

	state := State{}
	floor := 0
	itemMap := make(map[string]int)
	itemCount := 0

	for scanner.Scan() {
		line := scanner.Text()
		if strings.Contains(line, "nothing relevant") {
			floor++
			continue
		}
		items := strings.Split(line, " a ")
		for _, item := range items[1:] {
			parts := strings.Fields(item)
			element := parts[0]
			if strings.HasSuffix(element, "-compatible") {
				element = strings.TrimSuffix(element, "-compatible")
			}
			itemType := parts[1]

			if _, exists := itemMap[element]; !exists {
				itemMap[element] = itemCount
				itemCount++
			}

			if itemType == "generator" {
				state.floors[floor] |= 1 << (itemMap[element] + 8)
			} else {
				state.floors[floor] |= 1 << itemMap[element]
			}
		}
		floor++
	}
	return state
}

func main() {
	initial := parseInput("input.txt")
	fmt.Println("Part 1:", bfs(initial))

	// Part 2: Add new items to the first floor
	initial.floors[0] |= 0xF000 | 0xF0
	fmt.Println("Part 2:", bfs(initial))
}
