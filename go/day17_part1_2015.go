package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

// countCombinations counts the combinations of containers that can sum up to the target amount.
func countCombinations(containers []int, target, index int) int {
	if target == 0 {
		return 1
	}
	if target < 0 || index >= len(containers) {
		return 0
	}
	// Include current container and exclude current container
	return countCombinations(containers, target-containers[index], index+1) +
		countCombinations(containers, target, index+1)
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

	fmt.Println(countCombinations(containers, 150, 0))
}