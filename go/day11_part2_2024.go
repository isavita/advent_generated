package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// Function to trim leading zeros, ensuring at least one digit remains
func trimLeadingZeros(s string) string {
	i := 0
	for i < len(s)-1 && s[i] == '0' {
		i++
	}
	return s[i:]
}

// Function to split a string into two halves and trim leading zeros
func splitStone(s string) (string, string) {
	mid := len(s) / 2
	left := trimLeadingZeros(s[:mid])
	right := trimLeadingZeros(s[mid:])
	if left == "" {
		left = "0"
	}
	if right == "" {
		right = "0"
	}
	return left, right
}

// Function to multiply a string-represented number by 2024
func multiplyBy2024(s string) string {
	// Convert string to slice of runes for easier manipulation
	num := []rune(s)
	multiplier := []rune("2024")

	// Initialize result slice with zeros
	result := make([]int, len(num)+len(multiplier))
	for i := len(num)-1; i >= 0; i-- {
		carry := 0
		for j := len(multiplier)-1; j >= 0; j-- {
			product := (int(num[i]-'0') * int(multiplier[j]-'0')) + result[i+j+1] + carry
			result[i+j+1] = product % 10
			carry = product / 10
		}
		result[i] += carry
	}

	// Convert result back to string, trimming leading zeros
	start := 0
	for start < len(result)-1 && result[start] == 0 {
		start++
	}
	var sb strings.Builder
	for ; start < len(result); start++ {
		sb.WriteByte(byte(result[start] + '0'))
	}
	return sb.String()
}

func main() {
	// Open and read the input file
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening input.txt:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	if !scanner.Scan() {
		fmt.Println("Input file is empty")
		return
	}
	line := scanner.Text()
	stonesStr := strings.Fields(line)

	// Initialize the map with initial stone counts
	stonesMap := make(map[string]int64)
	for _, s := range stonesStr {
		stonesMap[s]++
	}

	// Perform 75 transformations
	const steps = 75
	for step := 0; step < steps; step++ {
		newStonesMap := make(map[string]int64)
		for stone, count := range stonesMap {
			if stone == "0" {
				// Rule 1: Replace 0 with 1
				newStonesMap["1"] += count
			} else if len(stone)%2 == 0 {
				// Rule 2: Split into two stones
				left, right := splitStone(stone)
				newStonesMap[left] += count
				newStonesMap[right] += count
			} else {
				// Rule 3: Multiply by 2024
				newStone := multiplyBy2024(stone)
				newStonesMap[newStone] += count
			}
		}
		stonesMap = newStonesMap
	}

	// Calculate the total number of stones
	var totalStones int64
	for _, count := range stonesMap {
		totalStones += count
	}

	fmt.Println(totalStones)
}
