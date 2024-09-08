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
	start, end uint32
}

func main() {
	ranges := readInput("input.txt")
	sort.Slice(ranges, func(i, j int) bool {
		return ranges[i].start < ranges[j].start
	})

	mergedRanges := mergeRanges(ranges)

	// Part 1
	lowestAllowed := uint32(0)
	if mergedRanges[0].start > 0 {
		lowestAllowed = 0
	} else {
		lowestAllowed = mergedRanges[0].end + 1
	}
	fmt.Printf("Part 1: %d\n", lowestAllowed)

	// Part 2
	allowed := countAllowed(mergedRanges)
	fmt.Printf("Part 2: %d\n", allowed)
}

func readInput(filename string) []IPRange {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)
	var ranges []IPRange
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), "-")
		start, _ := strconv.ParseUint(parts[0], 10, 32)
		end, _ := strconv.ParseUint(parts[1], 10, 32)
		ranges = append(ranges, IPRange{uint32(start), uint32(end)})
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

func countAllowed(ranges []IPRange) uint32 {
	var blocked, lastEnd uint32
	for _, r := range ranges {
		if r.start > lastEnd {
			blocked += r.end - r.start + 1
		} else if r.end > lastEnd {
			blocked += r.end - lastEnd
		}
		if r.end > lastEnd {
			lastEnd = r.end
		}
	}
	return 4294967296 - blocked // 2^32 - blocked
}
