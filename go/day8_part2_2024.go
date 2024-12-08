package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

// Compute gcd using Euclid's algorithm
func gcd(a, b int) int {
	if b == 0 {
		if a < 0 {
			return -a
		}
		return a
	}
	return gcd(b, a%b)
}

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

	// linesPerFreq: map frequency -> map of line signature -> true
	// line signature = (sx, sy, c), with direction normalized
	linesPerFreq := make(map[rune]map[string]bool)
	for f, coords := range antennas {
		linesPerFreq[f] = make(map[string]bool)
		n := len(coords)
		for i := 0; i < n; i++ {
			for j := i + 1; j < n; j++ {
				A := coords[i]
				B := coords[j]
				dy := B[0] - A[0]
				dx := B[1] - A[1]
				g := gcd(dy, dx)
				sy := dy / g
				sx := dx / g
				if sx < 0 || (sx == 0 && sy < 0) {
					sx = -sx
					sy = -sy
				}
				c := sy*A[1] - sx*A[0]
				key := strconv.Itoa(sx) + "," + strconv.Itoa(sy) + "," + strconv.Itoa(c)
				linesPerFreq[f][key] = true
			}
		}
	}

	// Now find all antinodes
	// An antinode occurs at any grid position in line with at least two antennas of the same frequency.
	// We've collected all lines for each frequency. For each line, we add all points on that line.

	antinodes := make(map[[2]int]bool)
	for _, lines := range linesPerFreq {
		for key := range lines {
			// parse sx, sy, c
			var sx, sy, c int
			fmt.Sscanf(key, "%d,%d,%d", &sx, &sy, &c)
			if sx == 0 && sy == 0 {
				continue
			}
			if sy == 0 {
				// horizontal line: -sx*y = c => y = -c/sx
				if c%sx == 0 {
					y := -c / sx
					if y >= 0 && y < h {
						for x := 0; x < w; x++ {
							antinodes[[2]int{y, x}] = true
						}
					}
				}
			} else if sx == 0 {
				// vertical line: sy*x = c => x = c/sy
				if c%sy == 0 {
					x := c / sy
					if x >= 0 && x < w {
						for y := 0; y < h; y++ {
							antinodes[[2]int{y, x}] = true
						}
					}
				}
			} else {
				// general case
				// sy*x - sx*y = c => sy*x = c + sx*y => x = (c + sx*y)/sy
				for y := 0; y < h; y++ {
					val := c + sx*y
					if val%sy == 0 {
						x := val / sy
						if x >= 0 && x < w {
							antinodes[[2]int{y, x}] = true
						}
					}
				}
			}
		}
	}

	fmt.Println(len(antinodes))
}
