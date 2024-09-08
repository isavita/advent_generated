package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var totalPart1, totalPart2 int

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Fields(line)
		springs, groups := parts[0], parseGroups(parts[1])

		totalPart1 += countArrangements(springs, groups)

		// Part 2: Unfold the input
		unfolded := strings.Repeat(springs+"?", 4) + springs
		unfoldedGroups := []int{}
		for i := 0; i < 5; i++ {
			unfoldedGroups = append(unfoldedGroups, groups...)
		}

		totalPart2 += countArrangements(unfolded, unfoldedGroups)
	}

	fmt.Println("Part 1:", totalPart1)
	fmt.Println("Part 2:", totalPart2)
}

func parseGroups(s string) []int {
	parts := strings.Split(s, ",")
	groups := make([]int, len(parts))
	for i, p := range parts {
		groups[i], _ = strconv.Atoi(p)
	}
	return groups
}

func countArrangements(springs string, groups []int) int {
	memo := make(map[string]int)
	return countArrangementsRecursive(springs, groups, 0, 0, 0, memo)
}

func countArrangementsRecursive(springs string, groups []int, springIndex, groupIndex, currentGroupSize int, memo map[string]int) int {
	key := fmt.Sprintf("%d,%d,%d", springIndex, groupIndex, currentGroupSize)
	if count, ok := memo[key]; ok {
		return count
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

	count := 0

	for _, c := range []byte{'.', '#'} {
		if springs[springIndex] == c || springs[springIndex] == '?' {
			if c == '.' && currentGroupSize == 0 {
				count += countArrangementsRecursive(springs, groups, springIndex+1, groupIndex, 0, memo)
			} else if c == '.' && currentGroupSize > 0 && groupIndex < len(groups) && groups[groupIndex] == currentGroupSize {
				count += countArrangementsRecursive(springs, groups, springIndex+1, groupIndex+1, 0, memo)
			} else if c == '#' {
				count += countArrangementsRecursive(springs, groups, springIndex+1, groupIndex, currentGroupSize+1, memo)
			}
		}
	}

	memo[key] = count
	return count
}
