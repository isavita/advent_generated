package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()

	var grid []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}

	h := len(grid)
	w := len(grid[0])
	antennas := make(map[rune][][2]int)
	for y := 0; y < h; y++ {
		for x := 0; x < w; x++ {
			c := rune(grid[y][x])
			if c != '.' {
				antennas[c] = append(antennas[c], [2]int{y, x})
			}
		}
	}

	antinodes := make(map[[2]int]bool)
	for _, coords := range antennas {
		n := len(coords)
		for i := 0; i < n; i++ {
			for j := i + 1; j < n; j++ {
				A := coords[i]
				B := coords[j]
				P1 := [2]int{2*A[0] - B[0], 2*A[1] - B[1]}
				P2 := [2]int{2*B[0] - A[0], 2*B[1] - A[1]}
				if P1[0] >= 0 && P1[0] < h && P1[1] >= 0 && P1[1] < w {
					antinodes[P1] = true
				}
				if P2[0] >= 0 && P2[0] < h && P2[1] >= 0 && P2[1] < w {
					antinodes[P2] = true
				}
			}
		}
	}

	fmt.Println(len(antinodes))
}
