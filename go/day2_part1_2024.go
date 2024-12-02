package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatalf("Failed to open input file: %v", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	safeReportCount := 0

	for scanner.Scan() {
		line := scanner.Text()
		levels, err := parseLevels(line)
		if err != nil {
			log.Fatalf("Failed to parse levels: %v", err)
		}

		if isSafeReport(levels) {
			safeReportCount++
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatalf("Error reading input file: %v", err)
	}

	fmt.Println(safeReportCount)
}

func parseLevels(line string) ([]int, error) {
	fields := strings.Fields(line)
	levels := make([]int, len(fields))

	for i, field := range fields {
		level, err := strconv.Atoi(field)
		if err != nil {
			return nil, fmt.Errorf("invalid integer '%s': %v", field, err)
		}
		levels[i] = level
	}
	return levels, nil
}

func isSafeReport(levels []int) bool {
	if len(levels) < 2 {
		// Reports with less than 2 levels are considered unsafe
		return false
	}

	firstDiff := levels[1] - levels[0]
	if firstDiff == 0 {
		return false // Neither increasing nor decreasing
	}

	isIncreasing := firstDiff > 0

	for i := 0; i < len(levels)-1; i++ {
		diff := levels[i+1] - levels[i]

		if diff == 0 {
			return false // Levels are equal, which is invalid
		}

		if (isIncreasing && diff <= 0) || (!isIncreasing && diff >= 0) {
			return false // Direction changed or levels are equal
		}

		absDiff := int(math.Abs(float64(diff)))
		if absDiff < 1 || absDiff > 3 {
			return false // Difference not between 1 and 3 inclusive
		}
	}

	return true
}
