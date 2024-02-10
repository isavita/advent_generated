package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	adapters := []int{0}

	for scanner.Scan() {
		joltage, _ := strconv.Atoi(scanner.Text())
		adapters = append(adapters, joltage)
	}

	sort.Ints(adapters)
	adapters = append(adapters, adapters[len(adapters)-1]+3)

	fmt.Printf("%d\n", countArrangements(adapters))
}

func countArrangements(adapters []int) int64 {

	ways := make(map[int]int64)
	ways[0] = 1

	for i := 1; i < len(adapters); i++ {
		currentJoltage := adapters[i]
		for _, diff := range []int{1, 2, 3} {
			ways[currentJoltage] += ways[currentJoltage-diff]
		}
	}

	return ways[adapters[len(adapters)-1]]
}