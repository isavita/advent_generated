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

// Point represents a local coordinate
type Point struct {
	r, c int
}

// Piece is a list of points
type Piece []Point

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	// --- 1. Parsing ---
	shapes := make(map[int]Piece)
	var regionLines []string
	var currentShapeLines []string
	currentID := -1
	parsingShapes := true

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		// Heuristic to detect region lines: contains "x" and ":"
		if strings.Contains(line, "x") && strings.Contains(line, ":") {
			parsingShapes = false
		}

		if parsingShapes {
			if strings.HasSuffix(line, ":") {
				if currentID != -1 && len(currentShapeLines) > 0 {
					shapes[currentID] = parseShape(currentShapeLines)
				}
				idStr := strings.TrimSuffix(line, ":")
				currentID, _ = strconv.Atoi(idStr)
				currentShapeLines = []string{}
			} else {
				currentShapeLines = append(currentShapeLines, line)
			}
		} else {
			regionLines = append(regionLines, line)
		}
	}
	if currentID != -1 && len(currentShapeLines) > 0 {
		shapes[currentID] = parseShape(currentShapeLines)
	}

	// Precompute variations
	shapeVariations := make(map[int][]Piece)
	for id, s := range shapes {
		shapeVariations[id] = generateVariations(s)
	}
	
	// Add variation for the "Slack" piece (ID -1), which is just a single point
	// FIXED: Explicit type declaration for Point and Piece
	shapeVariations[-1] = []Piece{ {Point{0, 0}} }
	shapes[-1] = Piece{Point{0, 0}} // Base shape for size calc

	// --- 2. Solving ---
	solvedCount := 0

	for _, rLine := range regionLines {
		parts := strings.Split(rLine, ":")
		dims := strings.Split(parts[0], "x")
		width, _ := strconv.Atoi(dims[0])
		height, _ := strconv.Atoi(dims[1])

		// Parse counts
		countsStr := strings.Fields(parts[1])
		pieceCounts := make(map[int]int)
		totalArea := 0

		for id, s := range countsStr {
			c, _ := strconv.Atoi(s)
			if c > 0 {
				pieceCounts[id] = c
				totalArea += c * len(shapes[id])
			}
		}

		gridSize := width * height
		if totalArea > gridSize {
			continue // Impossible
		}

		// Add Slack as pieces of ID -1
		slack := gridSize - totalArea
		if slack > 0 {
			pieceCounts[-1] = slack
		}

		// Prepare sorted list of IDs for deterministic iteration
		// Sort by Piece Size DESCENDING
		var ids []int
		for id := range pieceCounts {
			ids = append(ids, id)
		}
		sort.Slice(ids, func(i, j int) bool {
			return len(shapes[ids[i]]) > len(shapes[ids[j]])
		})

		// Initialize grid
		grid := make([]bool, gridSize)

		if solve(height, width, grid, pieceCounts, ids, shapeVariations, shapes) {
			solvedCount++
		}
	}

	fmt.Printf("Number of regions that fit all presents: %d\n", solvedCount)
}

// solve attempts to fill the grid.
func solve(rows, cols int, grid []bool, counts map[int]int, ids []int, variations map[int][]Piece, shapes map[int]Piece) bool {
	// 1. Find the first empty cell (Top-Left)
	emptyIdx := -1
	for i := 0; i < len(grid); i++ {
		if !grid[i] {
			emptyIdx = i
			break
		}
	}

	// If no empty cells, we are done!
	if emptyIdx == -1 {
		return true
	}

	// Coordinate of the empty cell
	r, c := emptyIdx/cols, emptyIdx%cols

	// 2. Island Pruning
	// Before making a choice, check if the grid is already impossible.
	if !checkIslands(rows, cols, grid, counts, shapes) {
		return false
	}

	// 3. Try to place pieces
	for _, id := range ids {
		if counts[id] > 0 {
			counts[id]--
			
			// Try all orientations
			for _, p := range variations[id] {
				// Optimization: Check if this piece fits specifically at (r,c) anchored at piece's (0,0)
				if canPlace(rows, cols, grid, p, r, c) {
					place(cols, grid, p, r, c, true)
					
					if solve(rows, cols, grid, counts, ids, variations, shapes) {
						return true
					}
					
					place(cols, grid, p, r, c, false) // Backtrack
				}
			}
			
			counts[id]++
		}
	}

	return false
}

// checkIslands returns false if the grid state is effectively dead
func checkIslands(rows, cols int, grid []bool, counts map[int]int, shapes map[int]Piece) bool {
	// Find smallest remaining real piece size
	minRealSize := 999999
	hasRealPieces := false
	for id, count := range counts {
		if id != -1 && count > 0 {
			sz := len(shapes[id])
			if sz < minRealSize {
				minRealSize = sz
			}
			hasRealPieces = true
		}
	}

	// If no real pieces left, we just need to fill with slack. 
	if !hasRealPieces {
		return true
	}

	availableSlack := counts[-1]
	visited := make([]bool, len(grid))

	// Find connected components of empty cells
	for i := 0; i < len(grid); i++ {
		if !grid[i] && !visited[i] {
			// Flood fill to find size
			size := 0
			q := []int{i}
			visited[i] = true
			
			for len(q) > 0 {
				curr := q[0]
				q = q[1:]
				size++
				
				currR, currC := curr/cols, curr%cols
				
				// Neighbors
				// Up
				if currR > 0 {
					n := (currR-1)*cols + currC
					if !grid[n] && !visited[n] {
						visited[n] = true
						q = append(q, n)
					}
				}
				// Down
				if currR < rows-1 {
					n := (currR+1)*cols + currC
					if !grid[n] && !visited[n] {
						visited[n] = true
						q = append(q, n)
					}
				}
				// Left
				if currC > 0 {
					n := currR*cols + (currC-1)
					if !grid[n] && !visited[n] {
						visited[n] = true
						q = append(q, n)
					}
				}
				// Right
				if currC < cols-1 {
					n := currR*cols + (currC+1)
					if !grid[n] && !visited[n] {
						visited[n] = true
						q = append(q, n)
					}
				}
			}

			// If island size < min_real_piece, it MUST be filled by slack.
			if size < minRealSize {
				if availableSlack >= size {
					availableSlack -= size
				} else {
					return false // Not enough slack to fill this tiny hole
				}
			}
		}
	}
	return true
}

func canPlace(rows, cols int, grid []bool, p Piece, r, c int) bool {
	for _, pt := range p {
		nr, nc := r+pt.r, c+pt.c
		if nr < 0 || nr >= rows || nc < 0 || nc >= cols {
			return false
		}
		if grid[nr*cols+nc] {
			return false
		}
	}
	return true
}

func place(cols int, grid []bool, p Piece, r, c int, val bool) {
	for _, pt := range p {
		idx := (r+pt.r)*cols + (c + pt.c)
		grid[idx] = val
	}
}

// --- Shape Helpers ---

func parseShape(lines []string) Piece {
	var p Piece
	for r, line := range lines {
		for c, char := range line {
			if char == '#' {
				p = append(p, Point{r, c})
			}
		}
	}
	return normalize(p)
}

func normalize(p Piece) Piece {
	if len(p) == 0 {
		return p
	}
	minR, minC := p[0].r, p[0].c
	for _, pt := range p {
		if pt.r < minR { minR = pt.r }
		if pt.c < minC { minC = pt.c }
	}
	newP := make(Piece, len(p))
	for i, pt := range p {
		newP[i] = Point{pt.r - minR, pt.c - minC}
	}
	// Sort by row, then col
	sort.Slice(newP, func(i, j int) bool {
		if newP[i].r == newP[j].r {
			return newP[i].c < newP[j].c
		}
		return newP[i].r < newP[j].r
	})
	return newP
}

func generateVariations(base Piece) []Piece {
	unique := make(map[string]Piece)
	curr := base
	for i := 0; i < 4; i++ {
		norm := normalize(curr)
		unique[fingerprint(norm)] = norm
		
		f := flip(curr)
		normF := normalize(f)
		unique[fingerprint(normF)] = normF
		
		curr = rotate(curr)
	}
	var res []Piece
	for _, p := range unique {
		res = append(res, p)
	}
	return res
}

func rotate(p Piece) Piece {
	res := make(Piece, len(p))
	for i, pt := range p {
		res[i] = Point{pt.c, -pt.r}
	}
	return res
}

func flip(p Piece) Piece {
	res := make(Piece, len(p))
	for i, pt := range p {
		res[i] = Point{pt.r, -pt.c}
	}
	return res
}

func fingerprint(p Piece) string {
	var sb strings.Builder
	for _, pt := range p {
		sb.WriteString(fmt.Sprintf("%d,%d|", pt.r, pt.c))
	}
	return sb.String()
}
