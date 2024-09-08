package main

import (
	"bufio"
	"fmt"
	"os"
	"math/bits"
)

func main() {
	patterns := readInput()
	part1 := solvePart1(patterns)
	part2 := solvePart2(patterns)
	fmt.Println("Part 1:", part1)
	fmt.Println("Part 2:", part2)
}

func readInput() [][]string {
	scanner := bufio.NewScanner(os.Stdin)
	var patterns [][]string
	var currentPattern []string

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			if len(currentPattern) > 0 {
				patterns = append(patterns, currentPattern)
				currentPattern = nil
			}
		} else {
			currentPattern = append(currentPattern, line)
		}
	}

	if len(currentPattern) > 0 {
		patterns = append(patterns, currentPattern)
	}

	return patterns
}

func solvePart1(patterns [][]string) int {
	sum := 0
	for _, pattern := range patterns {
		sum += findReflection(pattern, -1)
	}
	return sum
}

func solvePart2(patterns [][]string) int {
	sum := 0
	for _, pattern := range patterns {
		original := findReflection(pattern, -1)
		sum += findSmudgedReflection(pattern, original)
	}
	return sum
}

func findReflection(pattern []string, ignore int) int {
	rows := len(pattern)
	cols := len(pattern[0])

	// Check for horizontal reflection
	for i := 1; i < rows; i++ {
		if i*100 == ignore {
			continue
		}
		if isHorizontalReflection(pattern, i) {
			return i * 100
		}
	}

	// Check for vertical reflection
	for i := 1; i < cols; i++ {
		if i == ignore {
			continue
		}
		if isVerticalReflection(pattern, i) {
			return i
		}
	}

	return 0
}

func isHorizontalReflection(pattern []string, row int) bool {
	for i := 0; i < row && row+i < len(pattern); i++ {
		if pattern[row-1-i] != pattern[row+i] {
			return false
		}
	}
	return true
}

func isVerticalReflection(pattern []string, col int) bool {
	for _, line := range pattern {
		for i := 0; i < col && col+i < len(line); i++ {
			if line[col-1-i] != line[col+i] {
				return false
			}
		}
	}
	return true
}

func findSmudgedReflection(pattern []string, original int) int {
	rows := len(pattern)
	cols := len(pattern[0])

	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			// Flip the bit at (r, c)
			flipped := make([]string, len(pattern))
			copy(flipped, pattern)
			flipped[r] = flipped[r][:c] + string(flip(rune(flipped[r][c]))) + flipped[r][c+1:]

			// Check for new reflection
			newReflection := findReflection(flipped, original)
			if newReflection != 0 && newReflection != original {
				return newReflection
			}
		}
	}

	return 0
}

func flip(r rune) rune {
	if r == '.' {
		return '#'
	}
	return '.'
}
