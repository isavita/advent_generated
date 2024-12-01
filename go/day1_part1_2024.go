package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatalf("Failed to open input.txt: %v", err)
	}
	defer file.Close()

	var left []int
	var right []int

	scanner := bufio.NewScanner(file)
	lineNumber := 0
	for scanner.Scan() {
		line := scanner.Text()
		lineNumber++
		// Split the line into fields
		fields := strings.Fields(line)
		if len(fields) != 2 {
			log.Fatalf("Invalid input format at line %d: expected 2 numbers, got %d", lineNumber, len(fields))
		}
		// Parse the first number
		leftNum, err := strconv.Atoi(fields[0])
		if err != nil {
			log.Fatalf("Invalid number in left list at line %d: %v", lineNumber, err)
		}
		// Parse the second number
		rightNum, err := strconv.Atoi(fields[1])
		if err != nil {
			log.Fatalf("Invalid number in right list at line %d: %v", lineNumber, err)
		}
		// Append to respective slices
		left = append(left, leftNum)
		right = append(right, rightNum)
	}

	if err := scanner.Err(); err != nil {
		log.Fatalf("Error reading input.txt: %v", err)
	}

	// Check if both lists have the same length
	if len(left) != len(right) {
		log.Fatalf("Lists have different lengths: left has %d elements, right has %d elements", len(left), len(right))
	}

	// Sort both lists
	sort.Ints(left)
	sort.Ints(right)

	// Calculate total distance
	totalDistance := 0
	for i := 0; i < len(left); i++ {
		diff := left[i] - right[i]
		if diff < 0 {
			diff = -diff
		}
		totalDistance += diff
	}

	// Output the result
	fmt.Println(totalDistance)
}
