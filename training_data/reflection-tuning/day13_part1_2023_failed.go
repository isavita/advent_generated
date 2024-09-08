package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
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

	totalSum := 0
	for _, pattern := range patterns {
		totalSum += findReflectionValue(pattern)
	}

	fmt.Println(totalSum)
}

func findReflectionValue(pattern []string) int {
	// Check for horizontal reflection
	for i := 1; i < len(pattern); i++ {
		if isHorizontalReflection(pattern, i) {
			return 100 * i
		}
	}

	// Check for vertical reflection
	for i := 1; i < len(pattern[0]); i++ {
		if isVerticalReflection(pattern, i) {
			return i
		}
	}

	return 0
}

func isHorizontalReflection(pattern []string, row int) bool {
	for i := 0; i < row && i+row < len(pattern); i++ {
		if pattern[row-1-i] != pattern[row+i] {
			return false
		}
	}
	return true
}

func isVerticalReflection(pattern []string, col int) bool {
	for _, line := range pattern {
		for i := 0; i < col && i+col < len(line); i++ {
			if line[col-1-i] != line[col+i] {
				return false
			}
		}
	}
	return true
}
