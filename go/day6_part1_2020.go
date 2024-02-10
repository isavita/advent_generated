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
	groupAnswers := make(map[rune]bool)

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			totalCount += len(groupAnswers)
			groupAnswers = make(map[rune]bool)
		} else {
			for _, question := range line {
				groupAnswers[question] = true
			}
		}
	}

	totalCount += len(groupAnswers)
	fmt.Println(totalCount)
}