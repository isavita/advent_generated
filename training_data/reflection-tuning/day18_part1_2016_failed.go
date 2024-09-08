package main

import (
	"fmt"
	"strings"
)

func isTrap(left, center, right bool) bool {
	return (left && center && !right) ||
		(!left && center && right) ||
		(left && !center && !right) ||
		(!left && !center && right)
}

func nextRow(current string) string {
	next := make([]byte, len(current))
	for i := range current {
		left := i > 0 && current[i-1] == '^'
		center := current[i] == '^'
		right := i < len(current)-1 && current[i+1] == '^'
		
		if isTrap(left, center, right) {
			next[i] = '^'
		} else {
			next[i] = '.'
		}
	}
	return string(next)
}

func countSafeTiles(start string, rows int) int {
	safe := strings.Count(start, ".")
	current := start
	
	for i := 1; i < rows; i++ {
		current = nextRow(current)
		safe += strings.Count(current, ".")
	}
	
	return safe
}

func main() {
	input := ".^^.^.^^^^" // Replace with your actual input
	totalRows := 40
	safeTiles := countSafeTiles(input, totalRows)
	fmt.Printf("Number of safe tiles in %d rows: %d\n", totalRows, safeTiles)
}
