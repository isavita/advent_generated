package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// floorDiv performs integer division flooring the result towards negative infinity.
// This is necessary because Go's default '/' operator truncates towards zero (e.g., -5/10 = 0),
// but we need standard mathematical floor division for the modulo logic to work (e.g., -5/10 = -1).
func floorDiv(a, b int) int {
	q := a / b
	if a < 0 && a%b != 0 {
		q--
	}
	return q
}

func main() {
	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// The dial starts at 50
	currentPos := 50
	totalZeroHits := 0
	const dialSize = 100

	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		// Parse instruction
		direction := line[0]
		amountStr := line[1:]
		amount, err := strconv.Atoi(amountStr)
		if err != nil {
			log.Fatalf("Invalid number format in line '%s': %v", line, err)
		}

		if direction == 'R' {
			// Rotating Right (increasing numbers).
			// In a linear space starting at currentPos, we move to currentPos + amount.
			// We cross 0 (or a multiple of 100) whenever the linear value divides by 100.
			// Since currentPos is always < 100 (normalized), floor(currentPos/100) is 0.
			// The number of new multiples of 100 reached is simply floor((currentPos + amount) / 100).
			hits := (currentPos + amount) / dialSize
			totalZeroHits += hits

			// Update position
			currentPos = (currentPos + amount) % dialSize

		} else if direction == 'L' {
			// Rotating Left (decreasing numbers).
			// We move linearly from currentPos down to currentPos - amount.
			// We pass a zero (multiple of 100) when the value equals k*100.
			// The number of multiples of 100 in the interval (currentPos - amount, currentPos]
			// can be calculated using floor division.
			// Specifically, we count multiples in the range [currentPos - amount, currentPos - 1].
			// Count = floor((High) / N) - floor((Low - 1) / N)
			// High = currentPos - 1
			// Low = currentPos - amount
			
			hits := floorDiv(currentPos-1, dialSize) - floorDiv(currentPos-amount-1, dialSize)
			totalZeroHits += hits

			// Update position
			currentPos = (currentPos - amount) % dialSize
			// Fix Go's negative modulo result
			if currentPos < 0 {
				currentPos += dialSize
			}

		} else {
			log.Fatalf("Unknown direction '%c' in line '%s'", direction, line)
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	// Print the answer
	fmt.Printf("The password is: %d\n", totalZeroHits)
}
