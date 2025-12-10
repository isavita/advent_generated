package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	total := 0

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		buttons, targets := parseLine(line)
		minPresses := solve(buttons, targets)
		total += minPresses
	}

	fmt.Println(total)
}

func parseLine(line string) ([][]int, []int) {
	buttonRe := regexp.MustCompile(`\(([^)]*)\)`)
	buttonMatches := buttonRe.FindAllStringSubmatch(line, -1)

	var buttons [][]int
	for _, match := range buttonMatches {
		var button []int
		if match[1] != "" {
			for _, s := range strings.Split(match[1], ",") {
				n, _ := strconv.Atoi(strings.TrimSpace(s))
				button = append(button, n)
			}
		}
		buttons = append(buttons, button)
	}

	targetRe := regexp.MustCompile(`\{([^}]*)\}`)
	targetMatch := targetRe.FindStringSubmatch(line)

	var targets []int
	for _, s := range strings.Split(targetMatch[1], ",") {
		n, _ := strconv.Atoi(strings.TrimSpace(s))
		targets = append(targets, n)
	}

	return buttons, targets
}

func solve(buttons [][]int, targets []int) int {
	numCounters := len(targets)
	numButtons := len(buttons)

	// Build coefficient matrix A where A[j][i] = 1 if button i affects counter j
	// We need to solve: A * x = targets, minimize sum(x), x >= 0

	// Create matrix for Gaussian elimination (augmented matrix)
	// Each row is [coefficients... | target]
	matrix := make([][]float64, numCounters)
	for j := 0; j < numCounters; j++ {
		matrix[j] = make([]float64, numButtons+1)
		matrix[j][numButtons] = float64(targets[j])
	}

	for i, btn := range buttons {
		for _, j := range btn {
			if j < numCounters {
				matrix[j][i] = 1
			}
		}
	}

	// Gaussian elimination to find pivot columns and free variables
	pivotCol := make([]int, numCounters) // pivotCol[row] = column of pivot in that row
	for i := range pivotCol {
		pivotCol[i] = -1
	}

	row := 0
	for col := 0; col < numButtons && row < numCounters; col++ {
		// Find pivot
		maxRow := row
		for r := row + 1; r < numCounters; r++ {
			if math.Abs(matrix[r][col]) > math.Abs(matrix[maxRow][col]) {
				maxRow = r
			}
		}

		if math.Abs(matrix[maxRow][col]) < 1e-9 {
			continue // No pivot in this column
		}

		// Swap rows
		matrix[row], matrix[maxRow] = matrix[maxRow], matrix[row]

		// Scale pivot row
		scale := matrix[row][col]
		for c := col; c <= numButtons; c++ {
			matrix[row][c] /= scale
		}

		// Eliminate other rows
		for r := 0; r < numCounters; r++ {
			if r != row && math.Abs(matrix[r][col]) > 1e-9 {
				factor := matrix[r][col]
				for c := col; c <= numButtons; c++ {
					matrix[r][c] -= factor * matrix[row][c]
				}
			}
		}

		pivotCol[row] = col
		row++
	}

	rank := row

	// Identify free variables (columns without pivots)
	isPivot := make([]bool, numButtons)
	pivotRows := make([]int, numButtons)
	for i := range pivotRows {
		pivotRows[i] = -1
	}
	for r := 0; r < rank; r++ {
		if pivotCol[r] >= 0 {
			isPivot[pivotCol[r]] = true
			pivotRows[pivotCol[r]] = r
		}
	}

	var freeVars []int
	for i := 0; i < numButtons; i++ {
		if !isPivot[i] {
			freeVars = append(freeVars, i)
		}
	}

	// Calculate upper bounds for each button
	maxPresses := make([]int, numButtons)
	for i, btn := range buttons {
		maxPresses[i] = math.MaxInt32
		for _, j := range btn {
			if j < numCounters && targets[j] < maxPresses[i] {
				maxPresses[i] = targets[j]
			}
		}
		if maxPresses[i] == math.MaxInt32 {
			maxPresses[i] = 0
		}
	}

	// Sort free variables by max presses (smallest first to prune early)
	sort.Slice(freeVars, func(i, j int) bool {
		return maxPresses[freeVars[i]] < maxPresses[freeVars[j]]
	})

	bestResult := math.MaxInt32

	// Given values for free variables, compute pivot variables
	computePivots := func(freeValues []int) ([]int, bool) {
		result := make([]int, numButtons)

		// Set free variable values
		for i, fv := range freeVars {
			result[fv] = freeValues[i]
		}

		// Compute pivot variables using back-substitution
		for r := rank - 1; r >= 0; r-- {
			col := pivotCol[r]
			if col < 0 {
				continue
			}

			val := matrix[r][numButtons]
			for c := col + 1; c < numButtons; c++ {
				val -= matrix[r][c] * float64(result[c])
			}

			// Check if integer
			intVal := int(math.Round(val))
			if math.Abs(val-float64(intVal)) > 1e-6 {
				return nil, false
			}
			if intVal < 0 {
				return nil, false
			}
			if intVal > maxPresses[col] {
				return nil, false
			}
			result[col] = intVal
		}

		return result, true
	}

	// Enumerate over free variables
	freeValues := make([]int, len(freeVars))

	var enumerate func(idx int, currentSum int)
	enumerate = func(idx int, currentSum int) {
		if currentSum >= bestResult {
			return
		}

		if idx == len(freeVars) {
			presses, valid := computePivots(freeValues)
			if valid {
				total := 0
				for _, p := range presses {
					total += p
				}
				if total < bestResult {
					bestResult = total
				}
			}
			return
		}

		fv := freeVars[idx]
		maxVal := maxPresses[fv]

		for v := 0; v <= maxVal; v++ {
			freeValues[idx] = v
			enumerate(idx+1, currentSum+v)
		}
	}

	enumerate(0, 0)

	if bestResult == math.MaxInt32 {
		return -1
	}
	return bestResult
}
