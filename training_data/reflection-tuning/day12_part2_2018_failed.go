package main

import (
	"fmt"
	"strings"
)

type State struct {
	plants    map[int]bool
	min, max  int
	rules     map[string]bool
}

func parseInput(input string) State {
	lines := strings.Split(input, "\n")
	initialState := strings.TrimPrefix(lines[0], "initial state: ")
	
	state := State{
		plants: make(map[int]bool),
		rules:  make(map[string]bool),
	}
	
	for i, ch := range initialState {
		if ch == '#' {
			state.plants[i] = true
			state.max = i
		}
	}
	
	for _, line := range lines[2:] {
		parts := strings.Split(line, " => ")
		state.rules[parts[0]] = parts[1] == "#"
	}
	
	return state
}

func (s *State) nextGeneration() {
	newPlants := make(map[int]bool)
	for i := s.min - 2; i <= s.max + 2; i++ {
		pattern := ""
		for j := i - 2; j <= i + 2; j++ {
			if s.plants[j] {
				pattern += "#"
			} else {
				pattern += "."
			}
		}
		if s.rules[pattern] {
			newPlants[i] = true
			if i < s.min {
				s.min = i
			}
			if i > s.max {
				s.max = i
			}
		}
	}
	s.plants = newPlants
}

func (s *State) sum() int {
	sum := 0
	for pot := range s.plants {
		sum += pot
	}
	return sum
}

func solve(input string, generations int64) int {
	state := parseInput(input)
	
	var gen int64
	var prevSum, prevDiff int
	
	for gen = 0; gen < generations; gen++ {
		sum := state.sum()
		diff := sum - prevSum
		
		if diff == prevDiff {
			// Pattern detected, calculate final sum
			remainingGens := generations - gen - 1
			return sum + int(remainingGens) * diff
		}
		
		prevSum = sum
		prevDiff = diff
		state.nextGeneration()
	}
	
	return state.sum()
}

func main() {
	input := `your puzzle input here`
	
	fmt.Println("Part 1:", solve(input, 20))
	fmt.Println("Part 2:", solve(input, 50000000000))
}
