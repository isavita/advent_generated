package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// Machine represents the configuration for a single factory machine
type Machine struct {
	Target  []int   // Target state of lights (0 or 1)
	Buttons [][]int // List of buttons, each containing indices of lights it toggles
}

func main() {
	machines, err := parseInput("input.txt")
	if err != nil {
		log.Fatalf("Failed to parse input: %v", err)
	}

	totalPresses := 0
	for _, m := range machines {
		minPresses := solveMachine(m)
		if minPresses == -1 {
			fmt.Println("Warning: No solution found for a machine (this shouldn't happen based on problem description)")
		} else {
			totalPresses += minPresses
		}
	}

	fmt.Printf("Fewest total button presses: %d\n", totalPresses)
}

// solveMachine sets up the linear system and solves for minimum presses
func solveMachine(m Machine) int {
	rows := len(m.Target)
	cols := len(m.Buttons)

	// Build augmented matrix [A | b]
	// Dimensions: rows x (cols + 1)
	// Matrix elements are 0 or 1 (GF(2))
	matrix := make([][]int, rows)
	for r := 0; r < rows; r++ {
		matrix[r] = make([]int, cols+1)
		// Set target vector (last column)
		matrix[r][cols] = m.Target[r]
		
		// Set button vectors (columns 0 to cols-1)
		for c, btn := range m.Buttons {
			for _, lightIndex := range btn {
				if lightIndex == r {
					matrix[r][c] = 1
				}
			}
		}
	}

	return gaussianEliminationMinWeight(matrix, rows, cols)
}

// gaussianEliminationMinWeight performs Gaussian elimination over GF(2)
// and brute-forces free variables to find the minimum Hamming weight solution.
func gaussianEliminationMinWeight(mat [][]int, R, C int) int {
	pivotRow := 0
	colIsPivot := make([]bool, C)
	
	// Forward Elimination to RREF
	for c := 0; c < C && pivotRow < R; c++ {
		// Find pivot in current column c starting from pivotRow
		sel := -1
		for r := pivotRow; r < R; r++ {
			if mat[r][c] == 1 {
				sel = r
				break
			}
		}

		if sel == -1 {
			continue // No pivot in this column, it's a free variable
		}

		// Swap rows to bring pivot to top
		mat[pivotRow], mat[sel] = mat[sel], mat[pivotRow]

		// Eliminate other entries in this column (make them 0)
		for r := 0; r < R; r++ {
			if r != pivotRow && mat[r][c] == 1 {
				// XOR row r with pivotRow
				for k := c; k <= C; k++ {
					mat[r][k] ^= mat[pivotRow][k]
				}
			}
		}

		colIsPivot[c] = true
		pivotRow++
	}

	// Check for inconsistency: Row of [0 0 ... 0 | 1]
	for r := pivotRow; r < R; r++ {
		if mat[r][C] == 1 {
			return -1 // No solution exists
		}
	}

	// Identify Free Variables
	var freeVars []int
	for c := 0; c < C; c++ {
		if !colIsPivot[c] {
			freeVars = append(freeVars, c)
		}
	}

	// Brute force all combinations of free variables
	// Since standard Gaussian Elimination produces RREF, 
	// pivot variables are functions of free variables.
	minWeight := math.MaxInt
	numFree := len(freeVars)
	limit := 1 << numFree // 2^numFree combinations

	for i := 0; i < limit; i++ {
		// Proposed solution vector
		x := make([]int, C)
		currentWeight := 0

		// Set Free Variables based on bits of i
		for j := 0; j < numFree; j++ {
			if (i>>j)&1 == 1 {
				x[freeVars[j]] = 1
				currentWeight++
			}
		}

		// Calculate Pivot Variables
		// In RREF, for a pivot row r with pivot at column pc:
		// x[pc] + sum(mat[r][f] * x[f]) = mat[r][C]
		// => x[pc] = mat[r][C] XOR sum(mat[r][f] * x[f])
		// We iterate rows where pivots exist
		currPivotRow := 0
		for c := 0; c < C; c++ {
			if colIsPivot[c] {
				// This column c is the pivot for row currPivotRow
				val := mat[currPivotRow][C]
				
				// Add contributions from free variables to the right
				// (Actually, due to RREF, we check all columns > c. 
				// Pivot cols > c have 0 coefficient in this row, 
				// so only free vars matter).
				for k := c + 1; k < C; k++ {
					if mat[currPivotRow][k] == 1 {
						val ^= x[k]
					}
				}
				
				x[c] = val
				if val == 1 {
					currentWeight++
				}
				currPivotRow++
			}
		}

		if currentWeight < minWeight {
			minWeight = currentWeight
		}
	}

	return minWeight
}

func parseInput(filename string) ([]Machine, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var machines []Machine
	scanner := bufio.NewScanner(file)

	// Regex to capture the initial diagram [...]
	reDiagram := regexp.MustCompile(`^\[([.#]+)\]`)
	// Regex to capture button definitions (...)
	reButton := regexp.MustCompile(`\(([\d,]+)\)`)

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		// Parse Diagram
		diagramMatch := reDiagram.FindStringSubmatch(line)
		if diagramMatch == nil {
			continue
		}
		diagramStr := diagramMatch[1]
		target := make([]int, len(diagramStr))
		for i, char := range diagramStr {
			if char == '#' {
				target[i] = 1
			} else {
				target[i] = 0
			}
		}

		// Parse Buttons
		var buttons [][]int
		buttonMatches := reButton.FindAllStringSubmatch(line, -1)
		for _, match := range buttonMatches {
			// match[1] is the content inside parens, e.g., "0,1,2"
			parts := strings.Split(match[1], ",")
			var btn []int
			for _, p := range parts {
				val, err := strconv.Atoi(strings.TrimSpace(p))
				if err != nil {
					continue
				}
				btn = append(btn, val)
			}
			buttons = append(buttons, btn)
		}

		machines = append(machines, Machine{
			Target:  target,
			Buttons: buttons,
		})
	}

	return machines, scanner.Err()
}
