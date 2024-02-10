package main

import (
	"bufio"
	"fmt"
	"os"
)

const totalRows = 400000 // Total number of rows to generate

func main() {
	firstRow := readFirstRow("input.txt")
	safeTilesCount := countSafeTiles(firstRow, totalRows)
	fmt.Println(safeTilesCount)
}

func readFirstRow(filename string) string {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	if scanner.Scan() {
		return scanner.Text()
	}

	panic("Failed to read the first row")
}

func countSafeTiles(firstRow string, totalRows int) int {
	currentRow := firstRow
	safeCount := countChar(currentRow, '.')

	for i := 1; i < totalRows; i++ {
		var nextRow string
		for j := 0; j < len(currentRow); j++ {
			if isTrap(j-1, j, j+1, currentRow) {
				nextRow += "^"
			} else {
				nextRow += "."
				safeCount++
			}
		}
		currentRow = nextRow
	}
	return safeCount
}

func isTrap(left, center, right int, row string) bool {
	l := safeIfOutOfBounds(left, row)
	c := row[center]
	r := safeIfOutOfBounds(right, row)

	return (l == '^' && c == '^' && r == '.') ||
		(c == '^' && r == '^' && l == '.') ||
		(l == '^' && c == '.' && r == '.') ||
		(r == '^' && c == '.' && l == '.')
}

func safeIfOutOfBounds(index int, row string) rune {
	if index < 0 || index >= len(row) {
		return '.'
	}
	return rune(row[index])
}

func countChar(str string, char rune) int {
	count := 0
	for _, c := range str {
		if c == char {
			count++
		}
	}
	return count
}