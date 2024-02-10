package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lines := []string{}
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	slopes := [][]int{
		{1, 1},
		{3, 1},
		{5, 1},
		{7, 1},
		{1, 2},
	}

	product := 1
	for _, slope := range slopes {
		treeCount := 0
		pos := 0
		for i := 0; i < len(lines); i += slope[1] {
			if lines[i][pos] == '#' {
				treeCount++
			}
			pos = (pos + slope[0]) % len(lines[i])
		}
		product *= treeCount
	}

	fmt.Println(product)
}