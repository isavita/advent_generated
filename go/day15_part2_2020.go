package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	startingNumbers := strings.Split(strings.TrimSpace(string(data)), ",")

	spoken := make(map[int]int)

	var lastSpoken int
	for i, number := range startingNumbers {
		if i == len(startingNumbers)-1 {
			lastSpoken, _ = strconv.Atoi(number)
		} else {
			num, _ := strconv.Atoi(number)
			spoken[num] = i + 1
		}
	}

	for turn := len(startingNumbers) + 1; turn <= 30000000; turn++ {

		var nextNumber int
		if lastTurn, ok := spoken[lastSpoken]; ok {
			nextNumber = turn - 1 - lastTurn
		}
		spoken[lastSpoken] = turn - 1
		lastSpoken = nextNumber
	}

	fmt.Println(lastSpoken)
}