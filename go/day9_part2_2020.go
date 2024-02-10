package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

const invalidNumber = 14360655

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

	for i := 0; i < len(numbers); i++ {
		sum := numbers[i]
		min := numbers[i]
		max := numbers[i]
		for j := i + 1; j < len(numbers); j++ {
			sum += numbers[j]
			if numbers[j] < min {
				min = numbers[j]
			}
			if numbers[j] > max {
				max = numbers[j]
			}
			if sum == invalidNumber {
				fmt.Println(min + max)
				return
			} else if sum > invalidNumber {
				break
			}
		}
	}
}