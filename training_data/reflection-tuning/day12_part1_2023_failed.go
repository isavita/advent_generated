package main

import (
	"fmt"
	"strings"
)

func countArrangements(springs string, groups []int) int {
	cache := make(map[string]int)
	return countArrangementsHelper(springs, groups, 0, 0, 0, cache)
}

func countArrangementsHelper(springs string, groups []int, springIndex, groupIndex, currentGroupSize int, cache map[string]int) int {
	key := fmt.Sprintf("%d,%d,%d", springIndex, groupIndex, currentGroupSize)
	if cachedResult, exists := cache[key]; exists {
		return cachedResult
	}

	if springIndex == len(springs) {
		if groupIndex == len(groups) && currentGroupSize == 0 {
			return 1
		}
		if groupIndex == len(groups)-1 && groups[groupIndex] == currentGroupSize {
			return 1
		}
		return 0
	}

	result := 0

	if springs[springIndex] == '.' || springs[springIndex] == '?' {
		if currentGroupSize == 0 {
			result += countArrangementsHelper(springs, groups, springIndex+1, groupIndex, 0, cache)
		} else if groupIndex < len(groups) && groups[groupIndex] == currentGroupSize {
			result += countArrangementsHelper(springs, groups, springIndex+1, groupIndex+1, 0, cache)
		}
	}

	if springs[springIndex] == '#' || springs[springIndex] == '?' {
		result += countArrangementsHelper(springs, groups, springIndex+1, groupIndex, currentGroupSize+1, cache)
	}

	cache[key] = result
	return result
}

func main() {
	input := `???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1`

	lines := strings.Split(input, "\n")
	totalArrangements := 0

	for _, line := range lines {
		parts := strings.Split(line, " ")
		springs := parts[0]
		groupsStr := strings.Split(parts[1], ",")
		groups := make([]int, len(groupsStr))
		for i, g := range groupsStr {
			fmt.Sscanf(g, "%d", &groups[i])
		}
		arrangements := countArrangements(springs, groups)
		totalArrangements += arrangements
	}

	fmt.Println("Total arrangements:", totalArrangements)
}
