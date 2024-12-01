package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
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
		// Parse the first number (left list)
		leftNum, err := strconv.Atoi(fields[0])
		if err != nil {
			log.Fatalf("Invalid number in left list at line %d: %v", lineNumber, err)
		}
		// Parse the second number (right list)
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

	// Create a map to count occurrences in the right list
	rightCount := make(map[int]int)
	for _, num := range right {
		rightCount[num]++
	}

	// Calculate similarity score
	similarityScore := 0
	for _, num := range left {
		count, exists := rightCount[num]
		if exists {
			similarityScore += num * count
		}
		// If the number does not exist in the right list, it contributes 0
	}

	// Output the result
	fmt.Println(similarityScore)
}
