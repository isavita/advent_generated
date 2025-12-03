package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
	"unicode"
)

func main() {
	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	totalJoltage := 0
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		// Calculate max joltage for this bank and add to total
		totalJoltage += calculateMaxJoltage(line)
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Total output joltage: %d\n", totalJoltage)
}

// calculateMaxJoltage finds the largest number formed by two digits d1, d2
// such that d1 appears before d2 in the string.
func calculateMaxJoltage(bank string) int {
	// Digits are 1-9 (per puzzle description), but we check 9 down to 0 to be safe.
	// We want to maximize (d1 * 10 + d2).
	// Strategy:
	// 1. Iterate d1 from 9 down to 0.
	// 2. Find the *first* occurrence of d1.
	// 3. If d1 exists and has characters following it, find the max digit (d2) in the remaining string.
	// 4. Since we iterate d1 downwards, the first valid pair we find is guaranteed to be the maximum.

	for d1 := 9; d1 >= 0; d1-- {
		// Convert integer d1 to its rune representation
		digitChar := rune('0' + d1)

		// Find index of the first occurrence of this digit
		idx := strings.IndexRune(bank, digitChar)

		// If the digit isn't found, or if it is the very last character
		// (meaning no second digit can follow it), skip to the next lower d1.
		if idx == -1 || idx == len(bank)-1 {
			continue
		}

		// We found a valid start digit. Now find the largest digit following it.
		suffix := bank[idx+1:]
		maxD2 := -1

		for _, r := range suffix {
			if unicode.IsDigit(r) {
				val := int(r - '0')
				if val > maxD2 {
					maxD2 = val
				}
				// Optimization: If we find a 9, we can't get any higher for the ones place.
				if maxD2 == 9 {
					break
				}
			}
		}

		// If we found a valid second digit, return the combined value.
		if maxD2 != -1 {
			return d1*10 + maxD2
		}
	}

	return 0
}
