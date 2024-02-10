package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}
	startingNumbers := strings.Split(strings.TrimSpace(string(input)), ",")

	lastSpoken := make(map[int]int)
	var lastNumber, nextNumber int

	for turn := 1; turn <= 2020; turn++ {
		if turn-1 < len(startingNumbers) {
			lastNumber, _ = strconv.Atoi(startingNumbers[turn-1])
			lastSpoken[lastNumber] = turn
			continue
		}
		if lastTurn, ok := lastSpoken[lastNumber]; ok && lastTurn != turn-1 {
			nextNumber = turn - 1 - lastTurn
		} else {
			nextNumber = 0
		}
		lastSpoken[lastNumber] = turn - 1
		lastNumber = nextNumber
	}

	fmt.Println(lastNumber)
}