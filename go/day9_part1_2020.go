package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

const preambleLength = 25

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var numbers []int

	for scanner.Scan() {
		n, _ := strconv.Atoi(scanner.Text())
		numbers = append(numbers, n)
	}

	for i := preambleLength; i < len(numbers); i++ {
		if !isValid(numbers[i], numbers[i-preambleLength:i]) {
			fmt.Println(numbers[i])
			break
		}
	}
}

func isValid(number int, previousNumbers []int) bool {
	seen := make(map[int]bool)
	for _, n := range previousNumbers {
		if seen[number-n] {
			return true
		}
		seen[n] = true
	}
	return false
}