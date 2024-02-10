package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()

	var positions []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		numbers := strings.Split(line, ",")
		for _, num_str := range numbers {
			num, err := strconv.Atoi(num_str)
			if err != nil {
				fmt.Println(err)
				return
			}
			positions = append(positions, num)
		}
	}

	sort.Ints(positions)

	min_fuel := int(^uint(0) >> 1)
	for i := positions[0]; i <= positions[len(positions)-1]; i++ {
		fuel := 0
		for _, pos := range positions {
			fuel += calculateNewFuel(pos, i)
		}
		if fuel < min_fuel {
			min_fuel = fuel
		}
	}
	fmt.Println(min_fuel)
}

func calculateNewFuel(currentPosition int, newPosition int) int {
	diff := abs(currentPosition - newPosition)
	return (diff * (diff + 1)) / 2
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}