package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	// Step 1: Read Input
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}

	strArr := strings.Fields(strings.TrimSpace(string(data)))
	var banks []int
	for _, num := range strArr {
		n, _ := strconv.Atoi(num)
		banks = append(banks, n)
	}

	// Step 2: Initialize Variables
	seen := make(map[string]bool)
	cycles := 0

	// Step 3: Redistribution Loop
	for {
		// Convert current banks state to string to store in set
		state := fmt.Sprintf("%v", banks)

		// Step 4: Check for Repeats
		if seen[state] {
			break
		}
		seen[state] = true

		// Find the bank with most blocks
		maxIndex := 0
		for i := 1; i < len(banks); i++ {
			if banks[i] > banks[maxIndex] {
				maxIndex = i
			}
		}

		// Perform redistribution
		blocks := banks[maxIndex]
		banks[maxIndex] = 0
		for i := 1; i <= blocks; i++ {
			banks[(maxIndex+i)%len(banks)]++
		}

		// Increment cycle counter
		cycles++
	}

	// Output
	fmt.Println("It takes", cycles, "redistribution cycles to reach a repeated configuration.")
}