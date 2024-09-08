package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type IPRange struct {
	start, end int
}

func main() {
	ranges := readInput("input.txt")
	sort.Slice(ranges, func(i, j int) bool {
		return ranges[i].start < ranges[j].start
	})

	mergedRanges := mergeRanges(ranges)
	lowestUnblockedIP := findLowestUnblockedIP(mergedRanges)

	fmt.Println(lowestUnblockedIP)
}

func readInput(filename string) []IPRange {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var ranges []IPRange
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), "-")
		start, _ := strconv.Atoi(parts[0])
		end, _ := strconv.Atoi(parts[1])
		ranges = append(ranges, IPRange{start, end})
	}
	return ranges
}

func mergeRanges(ranges []IPRange) []IPRange {
	var merged []IPRange
	for _, r := range ranges {
		if len(merged) == 0 || r.start > merged[len(merged)-1].end+1 {
			merged = append(merged, r)
		} else if r.end > merged[len(merged)-1].end {
			merged[len(merged)-1].end = r.end
		}
	}
	return merged
}

func findLowestUnblockedIP(ranges []IPRange) int {
	if ranges[0].start > 0 {
		return 0
	}
	for i := 0; i < len(ranges)-1; i++ {
		if ranges[i].end+1 < ranges[i+1].start {
			return ranges[i].end + 1
		}
	}
	return ranges[len(ranges)-1].end + 1
}
