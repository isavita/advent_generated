package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"unicode"
)

func main() {
	// Read the file and convert it into a 2D slice of runes.
	matrix, err := readFileToMatrix("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	// Calculate the sum of all part numbers.
	sum := sumOfPartNumbers(matrix)
	fmt.Println(sum)
}

// readFileToMatrix reads the file and converts it into a 2D slice of runes.
func readFileToMatrix(filePath string) ([][]rune, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var matrix [][]rune
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		matrix = append(matrix, []rune(scanner.Text()))
	}
	return matrix, scanner.Err()
}

// sumOfPartNumbers calculates the sum of all part numbers in the matrix.
func sumOfPartNumbers(matrix [][]rune) int {
	sum := 0
	visited := make([][]bool, len(matrix))
	for i := range visited {
		visited[i] = make([]bool, len(matrix[i]))
	}

	for y, row := range matrix {
		for x := range row {
			if !visited[y][x] && unicode.IsDigit(matrix[y][x]) {
				number, length := extractNumber(matrix, x, y)
				if isAdjacentToSymbol(matrix, x, y, length) {
					sum += number
				}
				// Mark all digits of this number as visited.
				for i := 0; i < length; i++ {
					visited[y][x+i] = true
				}
			}
		}
	}
	return sum
}

// extractNumber extracts the whole number starting from the given position and returns the number and its length.
func extractNumber(matrix [][]rune, x, y int) (int, int) {
	numberStr := ""
	for x < len(matrix[y]) && unicode.IsDigit(matrix[y][x]) {
		numberStr += string(matrix[y][x])
		x++
	}
	number, _ := strconv.Atoi(numberStr)
	return number, len(numberStr)
}

// isAdjacentToSymbol checks if the given position is adjacent to a symbol within the length of the number.
func isAdjacentToSymbol(matrix [][]rune, x, y, length int) bool {
	for i := 0; i < length; i++ {
		if checkAdjacent(matrix, x+i, y) {
			return true
		}
	}
	return false
}

// checkAdjacent checks adjacent cells around a given cell for symbols.
func checkAdjacent(matrix [][]rune, x, y int) bool {
	for dy := -1; dy <= 1; dy++ {
		for dx := -1; dx <= 1; dx++ {
			adjX, adjY := x+dx, y+dy
			if adjY >= 0 && adjY < len(matrix) && adjX >= 0 && adjX < len(matrix[adjY]) {
				if !unicode.IsDigit(matrix[adjY][adjX]) && matrix[adjY][adjX] != '.' {
					return true
				}
			}
		}
	}
	return false
}