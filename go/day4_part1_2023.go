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
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	totalPoints := 0
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " | ")
		winningNumbers := convertToIntSlice(parts[0])
		yourNumbers := convertToIntSlice(parts[1])
		totalPoints += calculatePoints(winningNumbers, yourNumbers)
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	fmt.Println(totalPoints)
}

func convertToIntSlice(str string) []int {
	var numbers []int
	for _, numStr := range strings.Fields(str) {
		num, _ := strconv.Atoi(numStr)
		numbers = append(numbers, num)
	}
	return numbers
}

func calculatePoints(winningNumbers, yourNumbers []int) int {
	points := 0
	for _, num := range yourNumbers {
		if contains(winningNumbers, num) {
			if points == 0 {
				points = 1
			} else {
				points *= 2
			}
		}
	}
	return points
}

func contains(slice []int, val int) bool {
	for _, item := range slice {
		if item == val {
			return true
		}
	}
	return false
}