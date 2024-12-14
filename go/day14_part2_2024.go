package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

// Robot represents a robot with its current position and velocity.
type Robot struct {
	x, y   int
	vx, vy int
}

// mod ensures that the result of a modulo operation is always non-negative.
func mod(a, b int) int {
	return (a%b + b) % b
}

// parseLine parses a line of input and returns a Robot.
func parseLine(line string) (Robot, error) {
	// Example line: p=0,4 v=3,-3
	re := regexp.MustCompile(`p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)`)
	matches := re.FindStringSubmatch(line)
	if len(matches) != 5 {
		return Robot{}, fmt.Errorf("invalid line format: %s", line)
	}
	x, err := strconv.Atoi(matches[1])
	if err != nil {
		return Robot{}, err
	}
	y, err := strconv.Atoi(matches[2])
	if err != nil {
		return Robot{}, err
	}
	vx, err := strconv.Atoi(matches[3])
	if err != nil {
		return Robot{}, err
	}
	vy, err := strconv.Atoi(matches[4])
	if err != nil {
		return Robot{}, err
	}
	return Robot{x, y, vx, vy}, nil
}

// moveRobots updates the positions of all robots with wrap-around.
func moveRobots(robots []Robot, sizeX, sizeY int) {
	for i := range robots {
		robots[i].x = mod(robots[i].x+robots[i].vx, sizeX)
		robots[i].y = mod(robots[i].y+robots[i].vy, sizeY)
	}
}

// countQuadrants counts the number of robots in each quadrant.
// Quadrants are defined based on the center of the grid.
// Robots exactly on the center lines are not counted.
func countQuadrants(robots []Robot, sizeX, sizeY int) [4]int {
	var counts [4]int
	centerX := sizeX / 2
	centerY := sizeY / 2

	for _, robot := range robots {
		x, y := robot.x, robot.y
		if x < centerX {
			if y < centerY {
				counts[0]++
			} else if y > centerY {
				counts[1]++
			}
		} else if x > centerX {
			if y < centerY {
				counts[2]++
			} else if y > centerY {
				counts[3]++
			}
		}
	}
	return counts
}

// hasNoOverlaps checks if all robots have unique positions.
func hasNoOverlaps(robots []Robot) bool {
	positionMap := make(map[[2]int]bool)
	for _, robot := range robots {
		pos := [2]int{robot.x, robot.y}
		if _, exists := positionMap[pos]; exists {
			return false
		}
		positionMap[pos] = true
	}
	return true
}

// drawGrid visualizes the robots on the grid.
// '#' represents a robot, and '.' represents an empty space.
func drawGrid(robots []Robot, sizeX, sizeY int) {
	// Create a map for quick lookup of robot positions
	gridMap := make(map[[2]int]bool)
	for _, robot := range robots {
		gridMap[[2]int{robot.x, robot.y}] = true
	}

	for y := 0; y < sizeY; y++ {
		line := ""
		for x := 0; x < sizeX; x++ {
			if gridMap[[2]int{x, y}] {
				line += "#"
			} else {
				line += "."
			}
		}
		fmt.Println(line)
	}
}

func main() {
	// Define the grid size
	sizeX := 101
	sizeY := 103

	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening input.txt: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	// Read and parse the input
	var robots []Robot
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		robot, err := parseLine(line)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error parsing line: %v\n", err)
			os.Exit(1)
		}
		robots = append(robots, robot)
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input.txt: %v\n", err)
		os.Exit(1)
	}

	// PART 1
	robotsPart1 := make([]Robot, len(robots))
	copy(robotsPart1, robots)

	for n := 0; n < 100; n++ {
		moveRobots(robotsPart1, sizeX, sizeY)
	}

	counts := countQuadrants(robotsPart1, sizeX, sizeY)
	// Calculate the safety factor by multiplying the counts
	safetyFactor := 1
	for _, c := range counts {
		safetyFactor *= c
	}
	fmt.Printf("Part 1 - Safety Factor after 100 seconds: %d\n", safetyFactor)

	// PART 2
	robotsPart2 := make([]Robot, len(robots))
	copy(robotsPart2, robots)

	seconds := 0
	for {
		if hasNoOverlaps(robotsPart2) {
			break
		}
		moveRobots(robotsPart2, sizeX, sizeY)
		seconds++
		// To prevent infinite loops, you might want to set a maximum number of iterations
		if seconds > 1000000 {
			fmt.Println("Exceeded maximum iterations without finding a unique position configuration.")
			os.Exit(1)
		}
	}
	fmt.Printf("Part 2 - Fewest seconds to display Easter egg: %d\n", seconds)
	fmt.Println("Final positions of robots:")
	drawGrid(robotsPart2, sizeX, sizeY)
}
