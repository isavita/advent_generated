package main

import (
	"fmt"
	"math"
)

type Point struct {
	x, y int
}

type Robot struct {
	position Point
	direction int
}

func (r *Robot) turn(direction int) {
	if direction == 0 {
		r.direction = (r.direction - 1 + 4) % 4
	} else {
		r.direction = (r.direction + 1) % 4
	}
}

func (r *Robot) move() {
	switch r.direction {
	case 0: // Up
		r.position.y++
	case 1: // Right
		r.position.x++
	case 2: // Down
		r.position.y--
	case 3: // Left
		r.position.x--
	}
}

func paintHull(program []int, startingColor int) map[Point]int {
	robot := &Robot{position: Point{0, 0}, direction: 0}
	painted := make(map[Point]int)
	painted[robot.position] = startingColor

	input := make(chan int, 1)
	output := make(chan int, 2)

	go runIntcode(program, input, output)

	for {
		// Provide input
		input <- painted[robot.position]

		// Get color to paint
		color, ok := <-output
		if !ok {
			break // Program has halted
		}

		// Paint the panel
		painted[robot.position] = color

		// Get direction to turn
		direction, ok := <-output
		if !ok {
			break // Program has halted
		}

		// Turn and move
		robot.turn(direction)
		robot.move()
	}

	return painted
}

func runIntcode(program []int, input <-chan int, output chan<- int) {
	// Implement your Intcode computer here
	// This is a placeholder implementation
	for i := 0; i < len(program); i += 2 {
		output <- program[i]
		output <- program[i+1]
	}
	close(output)
}

func main() {
	program := []int{1,0,0,0,99} // Replace with your actual Intcode program

	// Part 1
	painted := paintHull(program, 0)
	fmt.Printf("Part 1: Number of panels painted at least once: %d\n", len(painted))

	// Part 2
	painted = paintHull(program, 1)
	minX, maxX, minY, maxY := math.MaxInt32, math.MinInt32, math.MaxInt32, math.MinInt32
	for p := range painted {
		minX = min(minX, p.x)
		maxX = max(maxX, p.x)
		minY = min(minY, p.y)
		maxY = max(maxY, p.y)
	}

	fmt.Println("Part 2: Registration identifier:")
	for y := maxY; y >= minY; y-- {
		for x := minX; x <= maxX; x++ {
			if painted[Point{x, y}] == 1 {
				fmt.Print("#")
			} else {
				fmt.Print(" ")
			}
		}
		fmt.Println()
	}
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
