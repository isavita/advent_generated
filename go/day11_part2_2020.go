package main

import (
	"bufio"
	"fmt"
	"os"
)

type Point struct {
	x, y int
}

var directions = []Point{
	{-1, -1}, {0, -1}, {1, -1},
	{-1, 0} /*{0,0},*/, {1, 0},
	{-1, 1}, {0, 1}, {1, 1},
}

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
		seatingArea, stabilized = simulateSeatingPartTwo(seatingArea)
	}

	fmt.Println(countOccupiedSeats(seatingArea))
}

func simulateSeatingPartTwo(seatingArea [][]rune) ([][]rune, bool) {
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
				if countVisibleOccupied(seatingArea, i, j) == 0 {
					newSeatingArea[i][j] = '#'
					stabilized = false
				}
			case '#':
				if countVisibleOccupied(seatingArea, i, j) >= 5 {
					newSeatingArea[i][j] = 'L'
					stabilized = false
				}
			}
		}
	}

	return newSeatingArea, stabilized
}

func countVisibleOccupied(seatingArea [][]rune, row, col int) int {
	count := 0
	for _, dir := range directions {
		for r, c := row+dir.y, col+dir.x; r >= 0 && r < len(seatingArea) && c >= 0 && c < len(seatingArea[0]); r, c = r+dir.y, c+dir.x {
			if seatingArea[r][c] == 'L' {
				break
			}
			if seatingArea[r][c] == '#' {
				count++
				break
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