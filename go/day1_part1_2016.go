package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Position struct {
	x, y     int
	dirIndex int
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

	pos := Position{0, 0, 0}                                  // Starting facing North
	directions := [4][2]int{{0, 1}, {1, 0}, {0, -1}, {-1, 0}} // North, East, South, West

	for _, instruction := range instructions {
		turn := instruction[:1]
		blocks, _ := strconv.Atoi(instruction[1:])

		if turn == "R" {
			pos.dirIndex = (pos.dirIndex + 1) % 4
		} else {
			pos.dirIndex = (pos.dirIndex - 1 + 4) % 4
		}

		pos.x += directions[pos.dirIndex][0] * blocks
		pos.y += directions[pos.dirIndex][1] * blocks
	}

	fmt.Println(abs(pos.x) + abs(pos.y))
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}