package main

import (
	"fmt"
	"math"
)

type Position struct {
	x, y int
}

func parseRegex(regex string) [][]string {
	var result [][]string
	var current []string
	stack := [][]string{}

	for i := 1; i < len(regex)-1; i++ {
		switch regex[i] {
		case '(':
			stack = append(stack, current)
			current = []string{}
		case ')':
			if len(stack) > 0 {
				prev := stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				if len(current) > 0 {
					result = append(result, current)
				}
				current = prev
			}
		case '|':
			if len(current) > 0 {
				result = append(result, current)
				current = []string{}
			}
		default:
			current = append(current, string(regex[i]))
		}
	}

	if len(current) > 0 {
		result = append(result, current)
	}

	return result
}

func buildMap(regex string) map[Position]int {
	facility := make(map[Position]int)
	current := Position{0, 0}
	facility[current] = 0

	paths := parseRegex(regex)
	for _, path := range paths {
		explorePath(facility, current, path)
	}

	return facility
}

func explorePath(facility map[Position]int, start Position, path []string) {
	current := start
	for _, dir := range path {
		switch dir {
		case "N":
			current.y++
		case "S":
			current.y--
		case "E":
			current.x++
		case "W":
			current.x--
		}
		if _, exists := facility[current]; !exists {
			facility[current] = math.MaxInt32
		}
		facility[current] = min(facility[current], facility[start]+1)
		start = current
	}
}

func findFurthestRoom(facility map[Position]int) int {
	maxDoors := 0
	for _, doors := range facility {
		if doors > maxDoors {
			maxDoors = doors
		}
	}
	return maxDoors
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func main() {
	regex := "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
	facility := buildMap(regex)
	furthestRoom := findFurthestRoom(facility)
	fmt.Println(furthestRoom)
}
