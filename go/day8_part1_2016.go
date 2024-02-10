package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

const (
	screenWidth  = 50
	screenHeight = 6
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	screen := make([][]bool, screenHeight)
	for i := range screen {
		screen[i] = make([]bool, screenWidth)
	}

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		processInstruction(scanner.Text(), screen)
	}

	fmt.Println(countLitPixels(screen))
}

func processInstruction(instruction string, screen [][]bool) {
	rectRegex := regexp.MustCompile(`rect (\d+)x(\d+)`)
	rotateRowRegex := regexp.MustCompile(`rotate row y=(\d+) by (\d+)`)
	rotateColumnRegex := regexp.MustCompile(`rotate column x=(\d+) by (\d+)`)

	switch {
	case rectRegex.MatchString(instruction):
		matches := rectRegex.FindStringSubmatch(instruction)
		a, _ := strconv.Atoi(matches[1])
		b, _ := strconv.Atoi(matches[2])
		rect(screen, a, b)

	case rotateRowRegex.MatchString(instruction):
		matches := rotateRowRegex.FindStringSubmatch(instruction)
		a, _ := strconv.Atoi(matches[1])
		b, _ := strconv.Atoi(matches[2])
		rotateRow(screen, a, b)

	case rotateColumnRegex.MatchString(instruction):
		matches := rotateColumnRegex.FindStringSubmatch(instruction)
		a, _ := strconv.Atoi(matches[1])
		b, _ := strconv.Atoi(matches[2])
		rotateColumn(screen, a, b)
	}
}

func rect(screen [][]bool, a, b int) {
	for y := 0; y < b; y++ {
		for x := 0; x < a; x++ {
			screen[y][x] = true
		}
	}
}

func rotateRow(screen [][]bool, row, shift int) {
	temp := make([]bool, screenWidth)
	for i := range screen[row] {
		temp[(i+shift)%screenWidth] = screen[row][i]
	}
	screen[row] = temp
}

func rotateColumn(screen [][]bool, col, shift int) {
	temp := make([]bool, screenHeight)
	for i := range screen {
		temp[(i+shift)%screenHeight] = screen[i][col]
	}
	for i := range screen {
		screen[i][col] = temp[i]
	}
}

func countLitPixels(screen [][]bool) int {
	count := 0
	for _, row := range screen {
		for _, pixel := range row {
			if pixel {
				count++
			}
		}
	}
	return count
}