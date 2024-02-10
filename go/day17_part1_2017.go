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
	buffer := []int{0}
	currentPos := 0

	for i := 1; i <= 2017; i++ {
		currentPos = (currentPos + steps) % len(buffer)
		// Insert new element after the current position
		buffer = append(buffer[:currentPos+1], buffer[currentPos:]...)
		buffer[currentPos+1] = i
		// Update the current position
		currentPos++
	}

	// Find the value immediately after the last inserted value (2017)
	for i, val := range buffer {
		if val == 2017 {
			fmt.Println(buffer[(i+1)%len(buffer)])
			break
		}
	}
}