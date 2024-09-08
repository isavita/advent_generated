package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func calculatePresents(house int) int {
	presents := 0
	for i := 1; i*i <= house; i++ {
		if house%i == 0 {
			presents += i * 10
			if i*i != house {
				presents += (house / i) * 10
			}
		}
	}
	return presents
}

func calculatePresentsPartTwo(house int) int {
	presents := 0
	for i := 1; i*i <= house; i++ {
		if house%i == 0 {
			if house/i <= 50 {
				presents += i * 11
			}
			if i <= 50 && i*i != house {
				presents += (house / i) * 11
			}
		}
	}
	return presents
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	input, _ := strconv.Atoi(scanner.Text())

	// Part 1
	house := 1
	for calculatePresents(house) < input {
		house++
	}
	fmt.Println("Part 1:", house)

	// Part 2
	house = 1
	for calculatePresentsPartTwo(house) < input {
		house++
	}
	fmt.Println("Part 2:", house)
}
