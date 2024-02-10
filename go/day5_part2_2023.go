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

// Reverse converts a number using binary search in the range mappings.
func reverseConvertNumber(number int, ranges []RangeMap) int {
	for i := len(ranges) - 1; i >= 0; i-- {
		r := ranges[i]
		if number >= r.destStart && number < r.destStart+r.length {
			return r.srcStart + (number - r.destStart)
		}
	}
	return number
}

// Checks if a number is within any of the given ranges.
func isInSeedRanges(number int, ranges [][2]int) bool {
	for _, r := range ranges {
		if number >= r[0] && number < r[0]+r[1] {
			return true
		}
	}
	return false
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var seedRanges [][2]int
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
			for i := 0; i < len(seedStrs); i += 2 {
				start, _ := strconv.Atoi(seedStrs[i])
				length, _ := strconv.Atoi(seedStrs[i+1])
				seedRanges = append(seedRanges, [2]int{start, length})
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
	if len(currentRanges) > 0 {
		maps = append(maps, currentRanges)
	}

	// Finding the lowest location number
	for location := 0; ; location++ {
		seed := location
		for i := len(maps) - 1; i >= 0; i-- {
			seed = reverseConvertNumber(seed, maps[i])
		}

		if isInSeedRanges(seed, seedRanges) {
			fmt.Println(location)
			break
		}
	}
}