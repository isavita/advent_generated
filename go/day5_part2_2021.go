package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func sign(x int) int {
	if x > 0 {
		return 1
	} else if x < 0 {
		return -1
	}
	return 0
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatalf("Failed to open input file: %s", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var lines [][4]int
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), " -> ")
		start := strings.Split(parts[0], ",")
		end := strings.Split(parts[1], ",")

		x1, _ := strconv.Atoi(start[0])
		y1, _ := strconv.Atoi(start[1])
		x2, _ := strconv.Atoi(end[0])
		y2, _ := strconv.Atoi(end[1])

		lines = append(lines, [4]int{x1, y1, x2, y2})
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("Error reading from input file: %s", err)
	}

	overlaps := make(map[[2]int]int)

	for _, line := range lines {
		x1, y1, x2, y2 := line[0], line[1], line[2], line[3]

		xStep := sign(x2 - x1)
		yStep := sign(y2 - y1)
		steps := abs(x2-x1) + 1
		if abs(y2-y1) > abs(x2-x1) {
			steps = abs(y2-y1) + 1
		}

		for i := 0; i < steps; i++ {
			point := [2]int{x1 + i*xStep, y1 + i*yStep}
			overlaps[point]++
		}
	}

	count := 0
	for _, v := range overlaps {
		if v > 1 {
			count++
		}
	}

	fmt.Println(count)
}