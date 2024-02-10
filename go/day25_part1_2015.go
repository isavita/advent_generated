package main

import (
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
)

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	// Use regular expression to extract the row and column from the input.
	re := regexp.MustCompile(`row (\d+), column (\d+)`)
	matches := re.FindStringSubmatch(string(data))
	if len(matches) != 3 {
		log.Fatal("Invalid input format.")
	}

	row, err := strconv.Atoi(matches[1])
	if err != nil {
		log.Fatalf("Invalid row number: %s", matches[1])
	}
	column, err := strconv.Atoi(matches[2])
	if err != nil {
		log.Fatalf("Invalid column number: %s", matches[2])
	}

	// Calculate the position in the code sequence for the given row and column
	pos := getPosition(row, column)

	// Calculate the code at the specified position
	code := getCode(pos)

	fmt.Printf("%d\n", code)
}

// getPosition calculates the position in the code sequence based on row and column.
func getPosition(row, column int) int {
	return (row+column-2)*(row+column-1)/2 + column
}

// getCode calculates the code at the given position.
func getCode(position int) int {
	const startCode = 20151125
	const multiplier = 252533
	const modulus = 33554393

	code := startCode
	for i := 1; i < position; i++ {
		code = (code * multiplier) % modulus
	}
	return code
}