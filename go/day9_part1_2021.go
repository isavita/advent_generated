package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var heightmap [][]int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]int, len(line))
		for i, char := range line {
			height, _ := strconv.Atoi(string(char))
			row[i] = height
		}
		heightmap = append(heightmap, row)
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	totalRiskLevel := 0
	for y, row := range heightmap {
		for x, height := range row {
			if isLowPoint(heightmap, x, y) {
				totalRiskLevel += 1 + height
			}
		}
	}

	fmt.Println(totalRiskLevel)
}

func isLowPoint(heightmap [][]int, x, y int) bool {
	height := heightmap[y][x]
	if x > 0 && heightmap[y][x-1] <= height {
		return false
	}
	if x < len(heightmap[y])-1 && heightmap[y][x+1] <= height {
		return false
	}
	if y > 0 && heightmap[y-1][x] <= height {
		return false
	}
	if y < len(heightmap)-1 && heightmap[y+1][x] <= height {
		return false
	}
	return true
}