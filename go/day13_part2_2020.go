package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func readBusIDs(fileName string) ([]int, []int, error) {
	file, err := os.Open(fileName)
	if err != nil {
		return nil, nil, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	// Skip the first line as it's not needed for Part Two
	scanner.Scan()
	scanner.Scan() // Read the second line
	busData := strings.Split(scanner.Text(), ",")

	var ids []int
	var offsets []int
	for i, bus := range busData {
		if bus != "x" {
			id, _ := strconv.Atoi(bus)
			ids = append(ids, id)
			offsets = append(offsets, i)
		}
	}
	return ids, offsets, nil
}

// Extended Euclidean Algorithm to find modular inverses
func extendedGCD(a, b int) (int, int) {
	if a == 0 {
		return 0, 1
	}
	x1, y1 := extendedGCD(b%a, a)
	x := y1 - (b/a)*x1
	y := x1
	return x, y
}

// Function to find the solution using the Chinese Remainder Theorem
func findEarliestTimestamp(ids, offsets []int) int {
	N := 1
	for _, id := range ids {
		N *= id
	}

	result := 0
	for i, id := range ids {
		ni := N / id
		xi, _ := extendedGCD(ni, id)
		result += (-offsets[i] + id) % id * xi * ni
	}
	return result % N
}

func main() {
	ids, offsets, err := readBusIDs("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	timestamp := findEarliestTimestamp(ids, offsets)
	fmt.Println(timestamp)
}