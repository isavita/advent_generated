package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Position struct {
	x, y int
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var instructions []string
	for scanner.Scan() {
		line := scanner.Text()
		instructions = strings.Split(line, ", ")
	}

	fmt.Println(firstRevisitedDistance(instructions))
}

func firstRevisitedDistance(instructions []string) int {
	pos := Position{0, 0}
	visited := make(map[Position]bool)
	visited[pos] = true
	directions := [4]Position{{0, 1}, {1, 0}, {0, -1}, {-1, 0}}
	dirIndex := 0 // Start facing North

	for _, instruction := range instructions {
		turn := instruction[:1]
		blocks, _ := strconv.Atoi(instruction[1:])

		if turn == "R" {
			dirIndex = (dirIndex + 1) % 4
		} else {
			dirIndex = (dirIndex - 1 + 4) % 4
		}

		for i := 0; i < blocks; i++ {
			pos.x += directions[dirIndex].x
			pos.y += directions[dirIndex].y

			if visited[pos] {
				return abs(pos.x) + abs(pos.y)
			}
			visited[pos] = true
		}
	}

	return -1 // No location visited twice
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}