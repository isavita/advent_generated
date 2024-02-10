package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	var seatingArea [][]rune
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		seatingArea = append(seatingArea, []rune(scanner.Text()))
	}

	stabilized := false
	for !stabilized {
		seatingArea, stabilized = simulateSeating(seatingArea)
	}

	fmt.Println(countOccupiedSeats(seatingArea))
}

func simulateSeating(seatingArea [][]rune) ([][]rune, bool) {
	rows := len(seatingArea)
	cols := len(seatingArea[0])
	newSeatingArea := make([][]rune, rows)
	for i := range newSeatingArea {
		newSeatingArea[i] = make([]rune, cols)
		copy(newSeatingArea[i], seatingArea[i])
	}
	stabilized := true

	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			switch seatingArea[i][j] {
			case 'L':
				if countAdjacentOccupied(seatingArea, i, j) == 0 {
					newSeatingArea[i][j] = '#'
					stabilized = false
				}
			case '#':
				if countAdjacentOccupied(seatingArea, i, j) >= 4 {
					newSeatingArea[i][j] = 'L'
					stabilized = false
				}
			}
		}
	}

	return newSeatingArea, stabilized
}

func countAdjacentOccupied(seatingArea [][]rune, row, col int) int {
	count := 0
	for i := row - 1; i <= row+1; i++ {
		for j := col - 1; j <= col+1; j++ {
			if i == row && j == col {
				continue
			}
			if i >= 0 && i < len(seatingArea) && j >= 0 && j < len(seatingArea[0]) {
				if seatingArea[i][j] == '#' {
					count++
				}
			}
		}
	}
	return count
}

func countOccupiedSeats(seatingArea [][]rune) int {
	count := 0
	for _, row := range seatingArea {
		for _, seat := range row {
			if seat == '#' {
				count++
			}
		}
	}
	return count
}