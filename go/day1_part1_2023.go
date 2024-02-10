package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"unicode"
)

func main() {
	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	var sum int
	scanner := bufio.NewScanner(file)

	// Process each line
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		firstDigit, lastDigit := -1, -1

		// Find the first and last digits in the line
		for _, r := range line {
			if unicode.IsDigit(r) {
				if firstDigit == -1 {
					firstDigit = int(r - '0')
				}
				lastDigit = int(r - '0')
			}
		}

		// Combine the digits and add to sum if both are found
		if firstDigit != -1 && lastDigit != -1 {
			value, _ := strconv.Atoi(fmt.Sprintf("%d%d", firstDigit, lastDigit))
			sum += value
		}
	}

	// Check for errors during scanning
	if err := scanner.Err(); err != nil {
		fmt.Println("Error scanning file:", err)
		return
	}

	// Output the result
	fmt.Println(sum)
}