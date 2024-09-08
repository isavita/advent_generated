package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	// Read input from file
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	target, _ := strconv.Atoi(scanner.Text())

	// Find the lowest house number
	house := 1
	for {
		presents := calculatePresents(house)
		if presents >= target {
			fmt.Println(house)
			break
		}
		house++
	}
}

func calculatePresents(house int) int {
	sum := 0
	for i := 1; i*i <= house; i++ {
		if house%i == 0 {
			sum += i * 10
			if i*i != house {
				sum += (house / i) * 10
			}
		}
	}
	return sum
}
