package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func parseInput(input []string) (histories [][]int) {
	histories = [][]int{}
	for _, line := range input {
		numbers := parseStringToInts(line)
		histories = append(histories, numbers)
	}
	return histories
}

func parseStringToInts(numbersLine string) []int {
	numbers := []int{}
	numbersParts := strings.Fields(numbersLine)
	for _, numberStr := range numbersParts {
		number, err := strconv.Atoi(numberStr)
		if err != nil {
			panic(err)
		}
		numbers = append(numbers, number)
	}
	return numbers
}

func allZeros(nums []int) bool {
	for _, num := range nums {
		if num != 0 {
			return false
		}
	}
	return true
}

func calculateExtrapolation(history []int) []int {
	extrapolations := []int{}
	for i := 1; i < len(history); i++ {
		extrapolation := history[i] - history[i-1]
		extrapolations = append(extrapolations, extrapolation)
	}
	return extrapolations
}

func calculateExtrapolations(history []int) [][]int {
	extrapolationsSeries := [][]int{}
	extrapolationsSeries = append(extrapolationsSeries, history)

	for i := 1; i < len(history); i++ {
		previousExtrapolations := extrapolationsSeries[i-1]
		if allZeros(previousExtrapolations) {
			return extrapolationsSeries
		}

		extrapolations := calculateExtrapolation(previousExtrapolations)
		extrapolationsSeries = append(extrapolationsSeries, extrapolations)
	}

	return extrapolationsSeries
}

func solve(input []string) int {
	histories := parseInput(input)
	res := 0

	for _, history := range histories {
		extrapolationsSeries := calculateExtrapolations(history)

		futurePrediction := 0
		for i := len(extrapolationsSeries) - 1; i > -1; i-- {
			futurePrediction = extrapolationsSeries[i][len(extrapolationsSeries[i])-1] + futurePrediction
		}

		res += futurePrediction
	}

	return res
}

func readFile(fileName string) []string {
	file, err := os.ReadFile(fileName)
	if err != nil {
		panic(err)
	}

	return strings.Split(strings.TrimSpace(string(file)), "\n")
}

func main() {
	input := readFile("input.txt")
	fmt.Println(solve(input))
}