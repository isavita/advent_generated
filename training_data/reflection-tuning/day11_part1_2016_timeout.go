package main

import (
	"fmt"
	"strings"
)

type State struct {
	floors  [4][]string
	elevator int
	steps   int
}

func isValid(floor []string) bool {
	generators := make(map[string]bool)
	microchips := make(map[string]bool)

	for _, item := range floor {
		if strings.HasSuffix(item, "G") {
			generators[item[:len(item)-1]] = true
		} else if strings.HasSuffix(item, "M") {
			microchips[item[:len(item)-1]] = true
		}
	}

	if len(generators) == 0 {
		return true
	}

	for chip := range microchips {
		if !generators[chip] {
			return false
		}
	}

	return true
}

func generateNextStates(current State) []State {
	var nextStates []State

	for _, direction := range []int{-1, 1} {
		nextFloor := current.elevator + direction
		if nextFloor < 0 || nextFloor > 3 {
			continue
		}

		for i := 0; i < len(current.floors[current.elevator]); i++ {
			for j := i; j < len(current.floors[current.elevator]); j++ {
				nextState := State{
					floors:   [4][]string{},
					elevator: nextFloor,
					steps:    current.steps + 1,
				}

				for k := 0; k < 4; k++ {
					nextState.floors[k] = append([]string{}, current.floors[k]...)
				}

				nextState.floors[nextFloor] = append(nextState.floors[nextFloor], current.floors[current.elevator][i])
				nextState.floors[current.elevator] = append(nextState.floors[current.elevator][:i], current.floors[current.elevator][i+1:]...)

				if i != j {
					nextState.floors[nextFloor] = append(nextState.floors[nextFloor], current.floors[current.elevator][j])
					nextState.floors[current.elevator] = append(nextState.floors[current.elevator][:j], current.floors[current.elevator][j+1:]...)
				}

				if isValid(nextState.floors[current.elevator]) && isValid(nextState.floors[nextFloor]) {
					nextStates = append(nextStates, nextState)
				}
			}
		}
	}

	return nextStates
}

func isGoalState(state State) bool {
	return len(state.floors[3]) == len(state.floors[0])+len(state.floors[1])+len(state.floors[2])+len(state.floors[3])
}

func solve(initial State) int {
	visited := make(map[string]bool)
	queue := []State{initial}

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if isGoalState(current) {
			return current.steps
		}

		stateKey := fmt.Sprintf("%v%d", current.floors, current.elevator)
		if visited[stateKey] {
			continue
		}
		visited[stateKey] = true

		nextStates := generateNextStates(current)
		queue = append(queue, nextStates...)
	}

	return -1
}

func main() {
	initial := State{
		floors: [4][]string{
			{"HM", "LM"},
			{"HG"},
			{"LG"},
			{},
		},
		elevator: 0,
		steps:    0,
	}

	steps := solve(initial)
	fmt.Println(steps)
}
