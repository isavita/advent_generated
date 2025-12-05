package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
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
	parsingRanges := true
	freshCount := 0

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())

		// Handle the blank line switch
		if line == "" {
			if parsingRanges {
				parsingRanges = false
				continue
			} else {
				// Skip empty lines in the ID section if any
				continue
			}
		}

		if parsingRanges {
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

			ranges = append(ranges, Range{Min: minVal, Max: maxVal})

		} else {
			// Parse ingredient ID
			id, err := strconv.Atoi(line)
			if err != nil {
				log.Fatalf("Invalid ID format: %s", line)
			}

			// Check if ID is in any range
			isFresh := false
			for _, r := range ranges {
				if id >= r.Min && id <= r.Max {
					isFresh = true
					break
				}
			}

			if isFresh {
				freshCount++
			}
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Number of fresh ingredients: %d\n", freshCount)
}
