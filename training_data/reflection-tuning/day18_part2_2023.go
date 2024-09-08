package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var x, y int64
	var area, perimeter int64
	
	for scanner.Scan() {
		line := scanner.Text()
		hex := line[len(line)-7 : len(line)-1]
		dist, _ := strconv.ParseInt(hex[:5], 16, 64)
		dir := hex[5]

		var dx, dy int64
		switch dir {
		case '0': // R
			dx = dist
		case '1': // D
			dy = dist
		case '2': // L
			dx = -dist
		case '3': // U
			dy = -dist
		}

		// Shoelace formula
		area += x*dy - y*dx
		// Perimeter
		perimeter += dist

		x += dx
		y += dy
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	// Area is half of the Shoelace result
	area = abs(area) / 2
	// Add perimeter/2 + 1 to account for the trench and starting point
	result := area + perimeter/2 + 1

	fmt.Println(result)
}

func abs(x int64) int64 {
	if x < 0 {
		return -x
	}
	return x
}
