package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Row struct {
	Springs string
	Group   []int
}

func parseInput(input []string) (rows []Row) {
	rows = []Row{}
	for _, line := range input {
		parts := strings.Split(line, " ")
		springs := parts[0]
		ints := parseStringToInts(parts[1])

		row := Row{
			Springs: springs,
			Group:   ints,
		}
		rows = append(rows, row)
	}
	return rows
}

func parseStringToInts(numbersLine string) []int {
	numbers := []int{}
	numbersParts := strings.Split(numbersLine, ",")
	for _, numberStr := range numbersParts {
		number, err := strconv.Atoi(numberStr)
		if err != nil {
			panic(err)
		}
		numbers = append(numbers, number)
	}
	return numbers
}

func countArrangementsRecursive(row Row, iSprings, iGroup, iContiguousDamaged int, cache map[[3]int]int) int {
	if iSprings == len(row.Springs) {
		if iGroup == len(row.Group) && iContiguousDamaged == 0 {
			return 1
		} else if iGroup == len(row.Group)-1 && iContiguousDamaged == row.Group[iGroup] {
			return 1
		}
		return 0
	}

	cacheKey := [3]int{iSprings, iGroup, iContiguousDamaged}
	if val, ok := cache[cacheKey]; ok {
		return val
	}

	res := 0
	char := row.Springs[iSprings]
	if char == '.' || char == '?' {
		if iContiguousDamaged == 0 {
			res += countArrangementsRecursive(row, iSprings+1, iGroup, iContiguousDamaged, cache)
		} else if iContiguousDamaged == row.Group[iGroup] {
			res += countArrangementsRecursive(row, iSprings+1, iGroup+1, 0, cache)
		}
	}
	if char == '#' || char == '?' {
		if iGroup < len(row.Group) && iContiguousDamaged < row.Group[iGroup] {
			res += countArrangementsRecursive(row, iSprings+1, iGroup, iContiguousDamaged+1, cache)
		}
	}

	cache[cacheKey] = res
	return res
}

func countArrangements(row Row) int {
	return countArrangementsRecursive(row, 0, 0, 0, map[[3]int]int{})
}

func unfoldRow(row Row, unfoldingFactor int) Row {
	newRow := Row{
		Springs: row.Springs,
		Group:   row.Group,
	}

	for i := 1; i < unfoldingFactor; i++ {
		newRow.Springs += "?" + row.Springs
		newRow.Group = append(newRow.Group, row.Group...)
	}

	return newRow
}

func solve(input []string) int {
	rows := parseInput(input)

	unfoldedRows := []Row{}
	for _, row := range rows {
		unfoldedRows = append(unfoldedRows, unfoldRow(row, 5))
	}

	res := 0
	for _, row := range unfoldedRows {
		res += countArrangements(row)
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