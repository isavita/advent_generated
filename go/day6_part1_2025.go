package main

import (
	"bufio"
	"fmt"
	"log"
	"math/big"
	"os"
	"strings"
	"unicode"
)

func main() {
	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// Read all lines to determine grid dimensions
	var lines []string
	scanner := bufio.NewScanner(file)
	maxWidth := 0
	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)
		if len(line) > maxWidth {
			maxWidth = len(line)
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	if len(lines) == 0 {
		fmt.Println("Grand total: 0")
		return
	}

	// 1. Identify separator columns (columns that are entirely whitespace)
	// isSep[x] will be true if column x contains only spaces (or is out of bounds)
	isSep := make([]bool, maxWidth)
	for x := 0; x < maxWidth; x++ {
		allSpaces := true
		for _, line := range lines {
			// If the line is long enough to have this column, check the character
			if x < len(line) && !unicode.IsSpace(rune(line[x])) {
				allSpaces = false
				break
			}
			// If line is shorter, it effectively has a space here
		}
		isSep[x] = allSpaces
	}

	grandTotal := new(big.Int)

	// 2. Iterate through columns to extract problems
	inBlock := false
	startCol := 0

	for x := 0; x < maxWidth; x++ {
		if !isSep[x] {
			// Found a non-empty column
			if !inBlock {
				inBlock = true
				startCol = x
			}
		} else {
			// Found a separator column
			if inBlock {
				// We just finished a problem block
				processBlock(lines, startCol, x-1, grandTotal)
				inBlock = false
			}
		}
	}
	// Handle case where the last problem goes to the edge of the file
	if inBlock {
		processBlock(lines, startCol, maxWidth-1, grandTotal)
	}

	fmt.Printf("Grand total: %s\n", grandTotal.String())
}

// processBlock parses a vertical slice of the file (from column start to end inclusive),
// extracts numbers and the operator, calculates the result, and adds it to grandTotal.
func processBlock(lines []string, start, end int, grandTotal *big.Int) {
	var nums []*big.Int
	var op string

	for _, line := range lines {
		// Handle jagged lines
		if start >= len(line) {
			continue
		}
		
		e := end + 1
		if e > len(line) {
			e = len(line)
		}
		
		// Extract the segment for this row
		segment := line[start:e]
		trimmed := strings.TrimSpace(segment)

		if trimmed == "" {
			continue
		}

		// Check if it is an operator
		if trimmed == "+" || trimmed == "*" {
			op = trimmed
		} else {
			// Parse number
			val := new(big.Int)
			_, ok := val.SetString(trimmed, 10)
			if ok {
				nums = append(nums, val)
			}
		}
	}

	if len(nums) == 0 {
		return
	}

	// Calculate result based on operator
	res := new(big.Int)
	if op == "+" {
		// Summation
		res.SetInt64(0)
		for _, n := range nums {
			res.Add(res, n)
		}
	} else if op == "*" {
		// Multiplication
		res.SetInt64(1)
		for _, n := range nums {
			res.Mul(res, n)
		}
	} else {
		// Fallback: if no operator is found but we have a number (e.g. single number),
		// treat it as the value. If multiple numbers and no op, it's ambiguous, 
		// but standard puzzle inputs usually guarantee well-formed problems.
		if len(nums) == 1 {
			res = nums[0]
		}
	}

	grandTotal.Add(grandTotal, res)
}
