package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type Ship struct {
	x, y   int
	facing int
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	ship := Ship{0, 0, 0}
	for scanner.Scan() {
		line := scanner.Text()
		action := line[0]
		value, _ := strconv.Atoi(line[1:])
		ship.processInstruction(rune(action), value)
	}

	manhattanDistance := abs(ship.x) + abs(ship.y)
	fmt.Println(manhattanDistance)
}

func (ship *Ship) processInstruction(action rune, value int) {
	switch action {
	case 'N':
		ship.y += value
	case 'S':
		ship.y -= value
	case 'E':
		ship.x += value
	case 'W':
		ship.x -= value
	case 'L':
		ship.facing = (ship.facing - value + 360) % 360
	case 'R':
		ship.facing = (ship.facing + value) % 360
	case 'F':
		switch ship.facing {
		case 0:
			ship.x += value
		case 90:
			ship.y -= value
		case 180:
			ship.x -= value
		case 270:
			ship.y += value
		}
	}
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}