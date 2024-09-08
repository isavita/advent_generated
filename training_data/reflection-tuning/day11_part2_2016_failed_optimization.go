package main

import (
	"fmt"
	"strings"
)

type State struct {
	elevator int
	floors   [4]uint16 // Bitset representation of items on each floor
}

func (s State) isValid() bool {
	for _, floor := range s.floors {
		generators := floor >> 8
		microchips := floor & 0xFF
		if microchips != 0 && generators != 0 && microchips&generators != microchips {
			return false
		}
	}
	return true
}

func (s State) isGoal() bool {
	return s.elevator == 3 && s.floors[0] == 0 && s.floors[1] == 0 && s.floors[2] == 0
}

func (s State) hash() string {
	return fmt.Sprintf("%d,%d,%d,%d,%d", s.elevator, s.floors[0], s.floors[1], s.floors[2], s.floors[3])
}

func bfs(initial State) int {
	queue := []State{initial}
	visited := make(map[string]bool)
	steps := 0

	for len(queue) > 0 {
		levelSize := len(queue)
		for i := 0; i < levelSize; i++ {
			current := queue[0]
			queue = queue[1:]

			if current.isGoal() {
				return steps
			}

			hash := current.hash()
			if visited[hash] {
				continue
			}
			visited[hash] = true

			for nextFloor := 0; nextFloor < 4; nextFloor++ {
				if nextFloor == current.elevator || abs(nextFloor-current.elevator) > 1 {
					continue
				}

				for item1 := 0; item1 < 16; item1++ {
					if current.floors[current.elevator]&(1<<item1) == 0 {
						continue
					}

					for item2 := item1; item2 < 16; item2++ {
						if item1 != item2 && current.floors[current.elevator]&(1<<item2) == 0 {
							continue
						}

						next := current
						next.elevator = nextFloor
						next.floors[current.elevator] &^= (1 << item1)
						next.floors[nextFloor] |= (1 << item1)

						if item1 != item2 {
							next.floors[current.elevator] &^= (1 << item2)
							next.floors[nextFloor] |= (1 << item2)
						}

						if next.isValid() {
							queue = append(queue, next)
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

func parseInput(input string) State {
	lines := strings.Split(input, "\n")
	var state State
	for i, line := range lines {
		for j, item := range []string{"promethium", "cobalt", "curium", "ruthenium", "plutonium"} {
			if strings.Contains(line, item+" generator") {
				state.floors[i] |= 1 << (j + 8)
			}
			if strings.Contains(line, item+"-compatible") {
				state.floors[i] |= 1 << j
			}
		}
	}
	return state
}

func main() {
	input := `The first floor contains a promethium generator and a promethium-compatible microchip.
The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
The fourth floor contains nothing relevant.`

	initialState := parseInput(input)
	steps := bfs(initialState)
	fmt.Println("Part 1:", steps)

	// Part 2: Add two more pairs
	initialState.floors[0] |= 0x0303 << 10 // Add elerium and dilithium generators and microchips
	steps = bfs(initialState)
	fmt.Println("Part 2:", steps)
}
