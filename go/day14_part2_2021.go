package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {
	template, rules := readInput("input.txt")
	pairCounts := make(map[string]int64)
	for i := 0; i < len(template)-1; i++ {
		pairCounts[template[i:i+2]]++
	}

	for step := 0; step < 40; step++ {
		newPairCounts := make(map[string]int64)
		for pair, count := range pairCounts {
			if insert, ok := rules[pair]; ok {
				newPairCounts[string(pair[0])+insert] += count
				newPairCounts[insert+string(pair[1])] += count
			} else {
				newPairCounts[pair] += count
			}
		}
		pairCounts = newPairCounts
	}

	elementCounts := make(map[rune]int64)
	for pair, count := range pairCounts {
		elementCounts[rune(pair[0])] += count
	}
	// Increment the count of the last element of the original template
	elementCounts[rune(template[len(template)-1])]++

	var maxCount, minCount int64 = 0, 1<<63 - 1
	for _, count := range elementCounts {
		if count > maxCount {
			maxCount = count
		}
		if count < minCount {
			minCount = count
		}
	}

	fmt.Println(maxCount - minCount)
}

func readInput(filename string) (string, map[string]string) {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	template := scanner.Text()

	rules := make(map[string]string)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		parts := strings.Split(line, " -> ")
		rules[parts[0]] = parts[1]
	}

	return template, rules
}