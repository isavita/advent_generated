package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type ipRange struct {
	start uint32
	end   uint32
}

func main() {
	ipRanges := readIPRanges("input.txt")
	sort.Slice(ipRanges, func(i, j int) bool {
		return ipRanges[i].start < ipRanges[j].start
	})

	unblockedIP := findUnblockedIP(ipRanges)
	fmt.Println(unblockedIP)
}

func readIPRanges(filename string) []ipRange {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var ranges []ipRange
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), "-")
		start, _ := strconv.ParseUint(parts[0], 10, 32)
		end, _ := strconv.ParseUint(parts[1], 10, 32)
		ranges = append(ranges, ipRange{start: uint32(start), end: uint32(end)})
	}
	return ranges
}

func findUnblockedIP(ranges []ipRange) uint32 {
	var currentIP uint32 = 0
	for _, r := range ranges {
		if r.start > currentIP {
			return currentIP
		}
		if r.end >= currentIP {
			currentIP = r.end + 1
		}
	}
	return currentIP
}