package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func knotHash(lengths []int, listSize int) []int {
	list := make([]int, listSize)
	for i := range list {
		list[i] = i
	}

	currentPos := 0
	skipSize := 0

	for _, length := range lengths {
		// Reverse the sublist
		for i := 0; i < length/2; i++ {
			a, b := (currentPos+i)%listSize, (currentPos+length-1-i)%listSize
			list[a], list[b] = list[b], list[a]
		}

		// Move the current position
		currentPos = (currentPos + length + skipSize) % listSize
		skipSize++
	}

	return list
}

func main() {
	// Read input from file
	input, err := ioutil.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	// Parse input
	lengthStrs := strings.Split(strings.TrimSpace(string(input)), ",")
	lengths := make([]int, len(lengthStrs))
	for i, s := range lengthStrs {
		lengths[i], _ = strconv.Atoi(s)
	}

	// Perform knot hash
	result := knotHash(lengths, 256)

	// Calculate and print the result
	fmt.Println(result[0] * result[1])
}
