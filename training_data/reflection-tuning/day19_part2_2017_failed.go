package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type Point struct {
	x, y int
}

func main() {
	diagram := parseInput("input.txt")
	letters, steps := traverseDiagram(diagram)
	fmt.Printf("Part 1: %s\n", letters)
	fmt.Printf("Part 2: %d\n", steps)
}

func parseInput(filename string) [][]rune {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)
	var diagram [][]rune
	for scanner.Scan() {
		diagram = append(diagram, []rune(scanner.Text()))
	}
	return diagram
}

func traverseDiagram(diagram [][]rune) (string, int) {
	x, y := findStart(diagram)
	dx, dy := 0, 1 // Start moving down
	var letters strings.Builder
	steps := 0

	for {
		x, y = x+dx, y+dy
		steps++

		switch diagram[y][x] {
		case '|', '-':
			continue
		case '+':
			dx, dy = turn(diagram, x, y, dx, dy)
		case ' ':
			return letters.String(), steps - 1
		default:
			if diagram[y][x] >= 'A' && diagram[y][x] <= 'Z' {
				letters.WriteRune(diagram[y][x])
			}
		}
	}
}

func findStart(diagram [][]rune) (int, int) {
	for x, ch := range diagram[0] {
		if ch == '|' {
			return x, 0
		}
	}
	return -1, -1
}

func turn(diagram [][]rune, x, y, dx, dy int) (int, int) {
	directions := []Point{{0, 1}, {1, 0}, {0, -1}, {-1, 0}}
	for _, dir := range directions {
		if (dir.x != dx || dir.y != dy) && (dir.x != -dx || dir.y != -dy) {
			newX, newY := x+dir.x, y+dir.y
			if newY >= 0 && newY < len(diagram) && newX >= 0 && newX < len(diagram[newY]) {
				if diagram[newY][newX] != ' ' {
					return dir.x, dir.y
				}
			}
		}
	}
	return 0, 0
}
