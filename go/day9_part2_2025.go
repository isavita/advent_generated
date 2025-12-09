package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var points []Point
	scanner := bufio.NewScanner(file)

	// Use maps to collect unique coordinates efficiently
	uniqueXMap := make(map[int]bool)
	uniqueYMap := make(map[int]bool)

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Split(line, ",")
		if len(parts) != 2 {
			continue
		}
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])

		points = append(points, Point{x, y})
		uniqueXMap[x] = true
		uniqueYMap[y] = true
	}

	if len(points) == 0 {
		fmt.Println("No points found.")
		return
	}

	// Convert maps to sorted slices
	var uniqueX, uniqueY []int
	for x := range uniqueXMap {
		uniqueX = append(uniqueX, x)
	}
	for y := range uniqueYMap {
		uniqueY = append(uniqueY, y)
	}
	sort.Ints(uniqueX)
	sort.Ints(uniqueY)

	// Maps to convert real coord -> index in unique slice
	xMap := make(map[int]int)
	for i, x := range uniqueX {
		xMap[x] = i
	}
	yMap := make(map[int]int)
	for i, y := range uniqueY {
		yMap[y] = i
	}

	// Build Compressed Grid
	// Dimensions: 2 * len(unique) + 1 to handle padding and gaps
	// Index mapping:
	// Real coordinate uniqueX[i] maps to grid column 2*i + 1
	// The gap between uniqueX[i] and uniqueX[i+1] maps to grid column 2*i + 2
	// Grid column 0 and the last column are padding.
	
	gridW := 2*len(uniqueX) + 1
	gridH := 2*len(uniqueY) + 1

	// Weights represent the physical width/height of that grid row/col
	colWidths := make([]int64, gridW)
	rowHeights := make([]int64, gridH)

	// Fill widths
	colWidths[0] = 1 // Padding
	for i := 0; i < len(uniqueX); i++ {
		// The explicit coordinate
		colWidths[2*i+1] = 1 
		// The gap after it, if it exists
		if i < len(uniqueX)-1 {
			gap := uniqueX[i+1] - uniqueX[i] - 1
			if gap < 0 { gap = 0 }
			colWidths[2*i+2] = int64(gap)
		} else {
			colWidths[2*i+2] = 1 // Padding end
		}
	}

	// Fill heights
	rowHeights[0] = 1 // Padding
	for i := 0; i < len(uniqueY); i++ {
		rowHeights[2*i+1] = 1
		if i < len(uniqueY)-1 {
			gap := uniqueY[i+1] - uniqueY[i] - 1
			if gap < 0 { gap = 0 }
			rowHeights[2*i+2] = int64(gap)
		} else {
			rowHeights[2*i+2] = 1
		}
	}

	// The Grid
	// 0 = Unknown, 1 = Boundary, 2 = Outside
	grid := make([][]int8, gridH)
	for i := range grid {
		grid[i] = make([]int8, gridW)
	}

	// Helper to get grid coordinates for a real point
	toGrid := func(p Point) (int, int) {
		xi := xMap[p.x]
		yi := yMap[p.y]
		return 2*xi + 1, 2*yi + 1
	}

	// Draw Boundary
	count := len(points)
	for i := 0; i < count; i++ {
		p1 := points[i]
		p2 := points[(i+1)%count]

		gx1, gy1 := toGrid(p1)
		gx2, gy2 := toGrid(p2)

		if gx1 == gx2 {
			// Vertical line
			start, end := gy1, gy2
			if start > end {
				start, end = end, start
			}
			for y := start; y <= end; y++ {
				// Only mark as boundary if the segment actually exists (width > 0)
				// Though for the explicit coordinate lines, width is always 1
				if rowHeights[y] > 0 {
					grid[y][gx1] = 1
				}
			}
		} else {
			// Horizontal line
			start, end := gx1, gx2
			if start > end {
				start, end = end, start
			}
			for x := start; x <= end; x++ {
				if colWidths[x] > 0 {
					grid[gy1][x] = 1
				}
			}
		}
	}

	// Flood Fill from (0,0) to mark Outside
	// (0,0) corresponds to padding, so it is definitely outside.
	queue := []Point{{0, 0}}
	grid[0][0] = 2 // Mark Outside

	dirs := []Point{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}

	for len(queue) > 0 {
		curr := queue[0]
		queue = queue[1:]

		for _, d := range dirs {
			nx, ny := curr.x+d.x, curr.y+d.y

			if nx >= 0 && nx < gridW && ny >= 0 && ny < gridH {
				if grid[ny][nx] == 0 {
					// Check if this is a "zero width" gap
					// If width is 0, it doesn't effectively exist as a barrier or a space,
					// but our graph topology treats it as a node. 
					// We just traverse it.
					grid[ny][nx] = 2
					queue = append(queue, Point{nx, ny})
				}
			}
		}
	}

	// Build Prefix Sum of Valid Areas
	// A cell is valid if grid value is 1 (Boundary) or 0 (Inside). 
	// If 2 (Outside), area is 0.
	P := make([][]int64, gridH)
	for i := range P {
		P[i] = make([]int64, gridW)
	}

	for y := 0; y < gridH; y++ {
		for x := 0; x < gridW; x++ {
			var val int64 = 0
			if grid[y][x] != 2 {
				val = colWidths[x] * rowHeights[y]
			}

			prevLeft := int64(0)
			if x > 0 { prevLeft = P[y][x-1] }
			
			prevUp := int64(0)
			if y > 0 { prevUp = P[y-1][x] }
			
			prevDiag := int64(0)
			if x > 0 && y > 0 { prevDiag = P[y-1][x-1] }

			P[y][x] = val + prevLeft + prevUp - prevDiag
		}
	}

	getSum := func(x1, y1, x2, y2 int) int64 {
		if x1 > x2 { x1, x2 = x2, x1 }
		if y1 > y2 { y1, y2 = y2, y1 }

		total := P[y2][x2]
		left := int64(0)
		if x1 > 0 { left = P[y2][x1-1] }
		up := int64(0)
		if y1 > 0 { up = P[y1-1][x2] }
		diag := int64(0)
		if x1 > 0 && y1 > 0 { diag = P[y1-1][x1-1] }

		return total - left - up + diag
	}

	// Check pairs
	maxArea := int64(0)

	for i := 0; i < len(points); i++ {
		for j := i; j < len(points); j++ {
			p1 := points[i]
			p2 := points[j]

			// Calculate real area
			realW := int64(abs(p1.x - p2.x)) + 1
			realH := int64(abs(p1.y - p2.y)) + 1
			area := realW * realH

			if area <= maxArea {
				continue
			}

			// Get grid coordinates
			gx1, gy1 := toGrid(p1)
			gx2, gy2 := toGrid(p2)

			// Get sum of valid area from prefix sum
			validArea := getSum(gx1, gy1, gx2, gy2)

			if validArea == area {
				maxArea = area
			}
		}
	}

	fmt.Printf("Largest valid area: %d\n", maxArea)
}

func abs(a int) int {
	if a < 0 { return -a }
	return a
}
