package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	totalElves := readInput("input.txt")
	winner := findWinningElf(totalElves)
	fmt.Println(winner)
}

func readInput(filename string) int {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	if scanner.Scan() {
		totalElves, err := strconv.Atoi(scanner.Text())
		if err != nil {
			panic(err)
		}
		return totalElves
	}
	panic("Failed to read the input")
}

func findWinningElf(totalElves int) int {
	highestPowerOfTwo := 1
	for highestPowerOfTwo*2 <= totalElves {
		highestPowerOfTwo *= 2
	}
	return (totalElves-highestPowerOfTwo)*2 + 1
}