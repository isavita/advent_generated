package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
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

	basinSizes := []int{}
	visited := make(map[[2]int]bool)

	for y, row := range heightmap {
		for x := range row {
			if isLowPoint(heightmap, x, y) {
				size := exploreBasin(heightmap, x, y, visited)
				basinSizes = append(basinSizes, size)
			}
		}
	}

	sort.Sort(sort.Reverse(sort.IntSlice(basinSizes)))
	result := basinSizes[0] * basinSizes[1] * basinSizes[2]
	fmt.Println(result)
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

func exploreBasin(heightmap [][]int, x, y int, visited map[[2]int]bool) int {
	if visited[[2]int{x, y}] || heightmap[y][x] == 9 {
		return 0
	}
	visited[[2]int{x, y}] = true
	size := 1

	directions := [][]int{{0, -1}, {-1, 0}, {0, 1}, {1, 0}}
	for _, dir := range directions {
		newX, newY := x+dir[0], y+dir[1]
		if newX >= 0 && newX < len(heightmap[0]) && newY >= 0 && newY < len(heightmap) {
			size += exploreBasin(heightmap, newX, newY, visited)
		}
	}
	return size
}