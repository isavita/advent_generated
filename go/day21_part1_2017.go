package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	rules := make(map[string]string)

	file, _ := os.Open("input.txt")
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), " => ")
		rules[parts[0]] = parts[1]
	}

	grid := []string{
		".#.",
		"..#",
		"###",
	}

	for i := 0; i < 5; i++ {
		var newSize int
		var subSize int

		if len(grid)%2 == 0 {
			subSize = 2
			newSize = len(grid) / 2 * 3
		} else {
			subSize = 3
			newSize = len(grid) / 3 * 4
		}

		newGrid := make([]string, newSize)
		for x := 0; x < newSize; x++ {
			newGrid[x] = ""
		}

		for y := 0; y < len(grid); y += subSize {
			for x := 0; x < len(grid); x += subSize {
				var square []string
				for dy := 0; dy < subSize; dy++ {
					square = append(square, grid[y+dy][x:x+subSize])
				}
				newSquare := enhance(strings.Join(square, "/"), rules)
				for dy, row := range strings.Split(newSquare, "/") {
					newGrid[y/subSize*(subSize+1)+dy] += row
				}
			}
		}
		grid = newGrid
	}

	count := 0
	for _, row := range grid {
		for _, pixel := range row {
			if pixel == '#' {
				count++
			}
		}
	}
	fmt.Println(count)
}

func enhance(input string, rules map[string]string) string {
	for i := 0; i < 4; i++ {
		if output, ok := rules[input]; ok {
			return output
		}
		input = rotate(input)
	}
	input = flip(input)
	for i := 0; i < 4; i++ {
		if output, ok := rules[input]; ok {
			return output
		}
		input = rotate(input)
	}
	return ""
}

func rotate(input string) string {
	parts := strings.Split(input, "/")
	size := len(parts)
	newParts := make([]string, size)
	for x := 0; x < size; x++ {
		var newRow string
		for y := size - 1; y >= 0; y-- {
			newRow += string(parts[y][x])
		}
		newParts[x] = newRow
	}
	return strings.Join(newParts, "/")
}

func flip(input string) string {
	parts := strings.Split(input, "/")
	for i, part := range parts {
		parts[i] = reverse(part)
	}
	return strings.Join(parts, "/")
}

func reverse(input string) string {
	runes := []rune(input)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}