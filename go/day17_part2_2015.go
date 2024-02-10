package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func findCombinations(containers []int, target, index, count int, minCount *int, ways *int) {
	if target == 0 {
		if *minCount == 0 || count < *minCount {
			*minCount = count
			*ways = 1
		} else if count == *minCount {
			*ways++
		}
		return
	}
	if target < 0 || index >= len(containers) {
		return
	}
	// Include current container
	findCombinations(containers, target-containers[index], index+1, count+1, minCount, ways)
	// Exclude current container
	findCombinations(containers, target, index+1, count, minCount, ways)
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	var containers []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		size, err := strconv.Atoi(scanner.Text())
		if err != nil {
			fmt.Println("Error reading file:", err)
			return
		}
		containers = append(containers, size)
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	var minCount, ways int
	findCombinations(containers, 150, 0, 0, &minCount, &ways)
	fmt.Println(ways)
}