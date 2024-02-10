package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var forest []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		forest = append(forest, scanner.Text())
	}

	trees := countTrees(forest, 3, 1)
	fmt.Println(trees)
}

func countTrees(forest []string, right, down int) int {
	trees := 0
	x := 0
	width := len(forest[0])

	for y := 0; y < len(forest); y += down {
		if forest[y][x%width] == '#' {
			trees++
		}
		x += right
	}

	return trees
}