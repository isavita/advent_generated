package main

import (
	"fmt"
	"io/os"
	"math"
	"path"
	"runtime"
	"sort"
	"strings"
)

func main() {
	ans := firewall(readFile("./input.txt"))
	fmt.Println(ans)
}

func firewall(input string) int {
	var allBlockedRanges [][2]int
	for _, line := range strings.Split(input, "\n") {
		var r [2]int
		fmt.Sscanf(line, "%d-%d", &r[0], &r[1])
		allBlockedRanges = append(allBlockedRanges, r)
	}
	sort.Slice(allBlockedRanges, func(i, j int) bool {
		if allBlockedRanges[i][0] != allBlockedRanges[j][0] {
			return allBlockedRanges[i][0] < allBlockedRanges[j][0]
		}
		return allBlockedRanges[i][1] < allBlockedRanges[j][1]
	})

	// merge allBlockedRanges
	merged := [][2]int{[2]int{}}
	for _, r := range allBlockedRanges {
		endOfLastRange := merged[len(merged)-1][1]
		if endOfLastRange >= r[0]-1 {
			merged[len(merged)-1][1] = maxInt(endOfLastRange, r[1])
		} else {
			merged = append(merged, r)
		}
	}

	if merged[len(merged)-1][1] != math.MaxUint32 {
		merged = append(merged, [2]int{math.MaxUint32, 0})
	}

	var totalAllowed int
	for i := 1; i < len(merged); i++ {
		totalAllowed += merged[i][0] - merged[i-1][1] - 1
	}

	return totalAllowed
}

func readFile(pathFromCaller string) string {
	_, filename, _, ok := runtime.Caller(1)
	if !ok {
		panic("Not Found")
	}

	absolutePath := path.Join(path.Dir(filename), pathFromCaller)

	content, err := os.ReadFile(absolutePath)
	if err != nil {
		panic(err)
	}

	strContent := string(content)
	return strings.TrimRight(strContent, "\n")
}

func maxInt(a, b int) int {
	if a > b {
		return a
	}
	return b
}