package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	totalCount := 0
	groupAnswers := make(map[rune]int)
	groupSize := 0

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			for _, count := range groupAnswers {
				if count == groupSize {
					totalCount++
				}
			}
			groupAnswers = make(map[rune]int)
			groupSize = 0
		} else {
			groupSize++
			for _, question := range line {
				groupAnswers[question]++
			}
		}
	}

	for _, count := range groupAnswers {
		if count == groupSize {
			totalCount++
		}
	}

	fmt.Println(totalCount)
}