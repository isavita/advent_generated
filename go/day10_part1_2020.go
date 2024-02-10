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
	var adapters []int

	for scanner.Scan() {
		joltage, _ := strconv.Atoi(scanner.Text())
		adapters = append(adapters, joltage)
	}

	sort.Ints(adapters)
	joltDifferences := map[int]int{3: 1}
	previousJoltage := 0

	for _, adapter := range adapters {
		diff := adapter - previousJoltage
		joltDifferences[diff]++
		previousJoltage = adapter
	}

	product := joltDifferences[1] * joltDifferences[3]
	fmt.Printf("%d\n", product)
}