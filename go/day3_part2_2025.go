package main

import (
	"bufio"
	"fmt"
	"log"
	"math/big"
	"os"
	"strings"
)

func main() {
	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	totalSum := new(big.Int)
	scanner := bufio.NewScanner(file)

	// We need exactly 12 digits
	const targetLength = 12

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		if len(line) < targetLength {
			// If line is shorter than 12 chars, we can't select 12.
			// Assuming valid input per problem description, but handling safety.
			continue
		}

		// Find the largest subsequence of length 12
		bestStr := findMaxSubsequence(line, targetLength)

		// Convert to Big Int and add to total
		val, ok := new(big.Int).SetString(bestStr, 10)
		if !ok {
			log.Fatalf("Failed to parse number: %s", bestStr)
		}
		totalSum.Add(totalSum, val)
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Total output joltage: %s\n", totalSum.String())
}

// findMaxSubsequence returns the largest number possible formed by selecting
// exactly 'k' digits from 's' while maintaining their relative order.
func findMaxSubsequence(s string, k int) string {
	n := len(s)
	// We need to remove exactly (n - k) digits to get a string of length k.
	toRemove := n - k

	// Use a stack to build the result greedy-ly
	stack := make([]byte, 0, n)

	for i := 0; i < n; i++ {
		digit := s[i]

		// While we still need to remove digits, and the stack is not empty,
		// and the current digit is greater than the last digit we picked:
		// Pop the last digit (because the current one is better).
		for toRemove > 0 && len(stack) > 0 && stack[len(stack)-1] < digit {
			stack = stack[:len(stack)-1]
			toRemove--
		}

		stack = append(stack, digit)
	}

	// If we still have removals left (e.g., input was "9876"), truncate the end.
	// Or if we simply finished the loop, we might have more than k characters.
	// We only want the first k characters of the resulting stack.
	return string(stack[:k])
}
