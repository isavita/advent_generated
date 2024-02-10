package main

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

func main() {
	// Step 1: Read Input
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}

	lines := strings.Split(strings.TrimSpace(string(data)), "\n")

	// Step 2: Create Data Structure
	// Create a map to store which programs hold which others
	holderMap := make(map[string]bool)
	heldMap := make(map[string]bool)

	// Regular expression to match program names
	re := regexp.MustCompile(`[a-z]+`)

	for _, line := range lines {
		names := re.FindAllString(line, -1)
		holder := names[0]
		holderMap[holder] = true

		// If the program holds others, update the held map
		if len(names) > 1 {
			for _, name := range names[1:] {
				heldMap[name] = true
			}
		}
	}

	// Step 3: Find Root
	for holder := range holderMap {
		if !heldMap[holder] {
			fmt.Println(holder)
			return
		}
	}
}