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

	// Read all lines to memory
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
	isSep := make([]bool, maxWidth)
	for x := 0; x < maxWidth; x++ {
		allSpaces := true
		for _, line := range lines {
			if x < len(line) && !unicode.IsSpace(rune(line[x])) {
				allSpaces = false
				break
			}
		}
		isSep[x] = allSpaces
	}

	grandTotal := new(big.Int)

	// 2. Iterate through columns to extract blocks
	inBlock := false
	startCol := 0

	for x := 0; x < maxWidth; x++ {
		if !isSep[x] {
			if !inBlock {
				inBlock = true
				startCol = x
			}
		} else {
			if inBlock {
				// End of a block found at x-1
				processVerticalBlock(lines, startCol, x-1, grandTotal)
				inBlock = false
			}
		}
	}
	// Handle last block if it goes to the edge
	if inBlock {
		processVerticalBlock(lines, startCol, maxWidth-1, grandTotal)
	}

	fmt.Printf("Grand total: %s\n", grandTotal.String())
}

// processVerticalBlock parses a block using Part 2 rules:
// - Each column in the block represents a number (MSD at top).
// - The operator (+ or *) is found somewhere in the block (usually bottom).
func processVerticalBlock(lines []string, start, end int, grandTotal *big.Int) {
	var nums []*big.Int
	var operator string

	// Iterate over each column in the block
	for c := start; c <= end; c++ {
		var sb strings.Builder
		hasDigits := false

		// Scan top to bottom
		for _, line := range lines {
			if c >= len(line) {
				continue
			}
			ch := rune(line[c])

			if unicode.IsDigit(ch) {
				sb.WriteRune(ch)
				hasDigits = true
			} else if ch == '+' || ch == '*' {
				operator = string(ch)
			}
		}

		// If this column contained digits, parse them as a number
		if hasDigits {
			val := new(big.Int)
			val.SetString(sb.String(), 10)
			nums = append(nums, val)
		}
	}

	if len(nums) == 0 {
		return
	}

	// Calculate result
	res := new(big.Int)
	if operator == "*" {
		res.SetInt64(1)
		for _, n := range nums {
			res.Mul(res, n)
		}
	} else {
		// Default to addition if + or unstated
		res.SetInt64(0)
		for _, n := range nums {
			res.Add(res, n)
		}
	}

	grandTotal.Add(grandTotal, res)
}
