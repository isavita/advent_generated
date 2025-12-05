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

// Range represents an inclusive interval [Min, Max]
type Range struct {
	Min int
	Max int
}

func main() {
	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var ranges []Range
	scanner := bufio.NewScanner(file)

	// 1. Parse the ranges
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())

		// Stop reading when we hit the blank line (we don't need the IDs following it)
		if line == "" {
			break
		}

		// Parse "min-max"
		parts := strings.Split(line, "-")
		if len(parts) != 2 {
			log.Fatalf("Invalid range format: %s", line)
		}

		minVal, err1 := strconv.Atoi(parts[0])
		maxVal, err2 := strconv.Atoi(parts[1])

		if err1 != nil || err2 != nil {
			log.Fatalf("Invalid numbers in range: %s", line)
		}

		// Ensure Min <= Max (just in case input is flipped, though unlikely)
		if minVal > maxVal {
			minVal, maxVal = maxVal, minVal
		}

		ranges = append(ranges, Range{Min: minVal, Max: maxVal})
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	if len(ranges) == 0 {
		fmt.Println("Total fresh IDs: 0")
		return
	}

	// 2. Sort ranges by starting value
	sort.Slice(ranges, func(i, j int) bool {
		return ranges[i].Min < ranges[j].Min
	})

	// 3. Merge overlapping ranges and count
	totalCount := 0

	// Start with the first range
	currentMin := ranges[0].Min
	currentMax := ranges[0].Max

	for i := 1; i < len(ranges); i++ {
		next := ranges[i]

		if next.Min <= currentMax {
			// Overlap detected (or strictly adjacent inside).
			// Extend the current range if the next one goes further.
			// Note: We use strictly <= currentMax because we are looking for set union.
			// If next.Min is 5 and currentMax is 4, they are disjoint (4, then 5).
			if next.Max > currentMax {
				currentMax = next.Max
			}
		} else {
			// No overlap.
			// Add the count of the completed range to total.
			// Count = Max - Min + 1 (inclusive)
			totalCount += (currentMax - currentMin + 1)

			// Start a new current range
			currentMin = next.Min
			currentMax = next.Max
		}
	}

	// Add the final range
	totalCount += (currentMax - currentMin + 1)

	fmt.Printf("Total fresh IDs: %d\n", totalCount)
}
