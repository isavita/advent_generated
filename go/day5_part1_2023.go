package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// RangeMap represents a single range mapping.
type RangeMap struct {
	srcStart, destStart, length int
}

// Converts a number using the range mappings.
func convertNumber(number int, ranges []RangeMap) int {
	for _, r := range ranges {
		if number >= r.srcStart && number < r.srcStart+r.length {
			return r.destStart + (number - r.srcStart)
		}
	}
	return number
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var seeds []int
	var currentRanges []RangeMap
	maps := make([][]RangeMap, 0)

	for scanner.Scan() {
		line := scanner.Text()
		if strings.Contains(line, "map:") {
			if len(currentRanges) > 0 {
				maps = append(maps, currentRanges)
				currentRanges = nil
			}
		} else if strings.HasPrefix(line, "seeds:") {
			seedStrs := strings.Split(line[7:], " ")
			for _, s := range seedStrs {
				seed, _ := strconv.Atoi(s)
				seeds = append(seeds, seed)
			}
		} else {
			numbers := strings.Fields(line)
			if len(numbers) == 3 {
				srcStart, _ := strconv.Atoi(numbers[1])
				destStart, _ := strconv.Atoi(numbers[0])
				length, _ := strconv.Atoi(numbers[2])

				currentRanges = append(currentRanges, RangeMap{srcStart, destStart, length})
			}
		}
	}
	maps = append(maps, currentRanges)

	minLocation := -1
	for _, seed := range seeds {
		location := seed
		for _, m := range maps {
			location = convertNumber(location, m)
		}

		if minLocation == -1 || location < minLocation {
			minLocation = location
		}
	}

	fmt.Println(minLocation)
}