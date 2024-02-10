package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	// Read the puzzle input from the file.
	data, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	input := strings.TrimSpace(string(data))
	target, err := strconv.Atoi(input)
	if err != nil {
		log.Fatalf("Invalid input: %s", input)
	}
	target /= 11 // Since each elf delivers presents equal to eleven times its number.

	// Initialize a slice to hold the number of presents for each house.
	houses := make([]int, target+1)

	// Modified sieve where each elf delivers to at most 50 houses.
	for elf := 1; elf <= target; elf++ {
		for house := elf; house <= elf*50 && house <= target; house += elf {
			houses[house] += elf
		}
	}

	// Find the first house to meet or exceed the target presents.
	for houseNumber, presents := range houses {
		if presents >= target {
			fmt.Printf("%d\n", houseNumber)
			break
		}
	}
}