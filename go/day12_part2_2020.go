package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type Ship struct {
	x, y      int
	waypointX int
	waypointY int
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	ship := Ship{0, 0, 10, 1}
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
		ship.waypointY += value
	case 'S':
		ship.waypointY -= value
	case 'E':
		ship.waypointX += value
	case 'W':
		ship.waypointX -= value
	case 'L':
		ship.rotateWaypoint(-value)
	case 'R':
		ship.rotateWaypoint(value)
	case 'F':
		ship.x += ship.waypointX * value
		ship.y += ship.waypointY * value
	}
}

func (ship *Ship) rotateWaypoint(degrees int) {
	degrees = (degrees + 360) % 360
	switch degrees {
	case 90, -270:
		ship.waypointX, ship.waypointY = ship.waypointY, -ship.waypointX
	case 180, -180:
		ship.waypointX, ship.waypointY = -ship.waypointX, -ship.waypointY
	case 270, -90:
		ship.waypointX, ship.waypointY = -ship.waypointY, ship.waypointX
	}
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}