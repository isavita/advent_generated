package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	universe := readInput("input.txt")
	emptyRows, emptyCols := findEmptySpaces(universe)
	galaxies := findGalaxies(universe, emptyRows, emptyCols)
	sum := sumShortestPaths(galaxies)
	fmt.Println(sum)
}

func readInput(filename string) [][]rune {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var universe [][]rune
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		universe = append(universe, []rune(scanner.Text()))
	}
	return universe
}

func findEmptySpaces(universe [][]rune) ([]int, []int) {
	rows, cols := len(universe), len(universe[0])
	emptyRows, emptyCols := make([]int, rows), make([]int, cols)

	for i := 0; i < rows; i++ {
		isEmpty := true
		for j := 0; j < cols; j++ {
			if universe[i][j] == '#' {
				isEmpty = false
				break
			}
		}
		if isEmpty {
			emptyRows[i] = 1
		}
	}

	for j := 0; j < cols; j++ {
		isEmpty := true
		for i := 0; i < rows; i++ {
			if universe[i][j] == '#' {
				isEmpty = false
				break
			}
		}
		if isEmpty {
			emptyCols[j] = 1
		}
	}

	// Calculate cumulative expansions
	for i := 1; i < rows; i++ {
		emptyRows[i] += emptyRows[i-1]
	}
	for j := 1; j < cols; j++ {
		emptyCols[j] += emptyCols[j-1]
	}

	return emptyRows, emptyCols
}

func findGalaxies(universe [][]rune, emptyRows, emptyCols []int) [][2]int {
	var galaxies [][2]int
	for i, row := range universe {
		for j, cell := range row {
			if cell == '#' {
				galaxies = append(galaxies, [2]int{i + emptyRows[i], j + emptyCols[j]})
			}
		}
	}
	return galaxies
}

func sumShortestPaths(galaxies [][2]int) int {
	sum := 0
	for i := 0; i < len(galaxies); i++ {
		for j := i + 1; j < len(galaxies); j++ {
			sum += abs(galaxies[i][0]-galaxies[j][0]) + abs(galaxies[i][1]-galaxies[j][1])
		}
	}
	return sum
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}
