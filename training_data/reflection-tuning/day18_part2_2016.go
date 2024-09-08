package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func countSafeTiles(initialRow string, totalRows int) int {
	safeTiles := strings.Count(initialRow, ".")
	currentRow := initialRow

	for i := 1; i < totalRows; i++ {
		nextRow := make([]byte, len(currentRow))
		for j := 0; j < len(currentRow); j++ {
			left := byte('.')
			if j > 0 {
				left = currentRow[j-1]
			}
			right := byte('.')
			if j < len(currentRow)-1 {
				right = currentRow[j+1]
			}
			center := currentRow[j]

			if (left == '^' && center == '^' && right == '.') ||
				(left == '.' && center == '^' && right == '^') ||
				(left == '^' && center == '.' && right == '.') ||
				(left == '.' && center == '.' && right == '^') {
				nextRow[j] = '^'
			} else {
				nextRow[j] = '.'
				safeTiles++
			}
		}
		currentRow = string(nextRow)
	}

	return safeTiles
}

func main() {
	input, err := ioutil.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	initialRow := strings.TrimSpace(string(input))

	part1 := countSafeTiles(initialRow, 40)
	fmt.Println("Part 1:", part1)

	part2 := countSafeTiles(initialRow, 400000)
	fmt.Println("Part 2:", part2)
}
