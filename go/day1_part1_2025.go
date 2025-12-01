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
		log.Fatal(err)
	}
	defer file.Close()

	// The dial starts at 50
	currentPos := 50
	zeroCount := 0

	// Create a scanner to read the file line by line
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		// Parse the instruction
		// Format is like "L68" or "R48"
		direction := line[0] // 'L' or 'R'
		amountStr := line[1:]
		amount, err := strconv.Atoi(amountStr)
		if err != nil {
			log.Fatalf("Invalid number format in line '%s': %v", line, err)
		}

		// Update position based on direction
		if direction == 'R' {
			// Rotating Right adds to the position
			currentPos = (currentPos + amount) % 100
		} else if direction == 'L' {
			// Rotating Left subtracts from the position
			currentPos = (currentPos - amount) % 100
			
			// Handle negative modulo results in Go
			// e.g., -18 % 100 = -18, but we want 82
			if currentPos < 0 {
				currentPos += 100
			}
		} else {
			log.Fatalf("Unknown direction '%c' in line '%s'", direction, line)
		}

		// Check if the dial is pointing at 0
		if currentPos == 0 {
			zeroCount++
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	// Print the answer
	fmt.Printf("The password is: %d\n", zeroCount)
}
