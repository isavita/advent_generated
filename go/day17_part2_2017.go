package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}

	steps, _ := strconv.Atoi(strings.TrimSpace(string(data)))
	currentPos := 0
	valueAfterZero := 0

	for i := 1; i <= 50000000; i++ {
		// Calculate the position where the new element will be inserted
		currentPos = (currentPos + steps) % i
		// Check if the new element will be inserted immediately after 0
		if currentPos == 0 {
			valueAfterZero = i
		}
		// Update the current position for the next iteration
		currentPos++
	}

	fmt.Println(valueAfterZero)
}