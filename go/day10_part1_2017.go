package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	// Read input lengths from a file
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	lengthsStr := strings.Split(scanner.Text(), ",")
	var lengths []int
	for _, l := range lengthsStr {
		n, _ := strconv.Atoi(l)
		lengths = append(lengths, n)
	}

	// Initialize variables
	list := make([]int, 256)
	for i := 0; i < 256; i++ {
		list[i] = i
	}
	currentPosition := 0
	skipSize := 0

	// Perform the knot-tying operations
	for _, length := range lengths {
		// Reverse the elements
		for i := 0; i < length/2; i++ {
			start := (currentPosition + i) % 256
			end := (currentPosition + length - 1 - i) % 256
			list[start], list[end] = list[end], list[start]
		}

		// Move the current position and increase the skip size
		currentPosition = (currentPosition + length + skipSize) % 256
		skipSize++
	}

	// Multiply the first two numbers in the list
	result := list[0] * list[1]
	fmt.Println(result)
}