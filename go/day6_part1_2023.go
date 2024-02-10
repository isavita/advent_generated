package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// calculateWaysToWin calculates the number of ways to beat the record for a single race.
func calculateWaysToWin(time, record int) int {
	waysToWin := 0
	for holdTime := 1; holdTime < time; holdTime++ {
		travelTime := time - holdTime
		distance := holdTime * travelTime
		if distance > record {
			waysToWin++
		}
	}
	return waysToWin
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var times, distances []int

	// Assuming the file has two lines: the first for times and the second for distances
	for scanner.Scan() {
		line := scanner.Text()
		values := strings.Split(line, " ")
		var intValues []int
		for _, value := range values {
			intValue, err := strconv.Atoi(value)
			if err != nil {
				continue
			}
			intValues = append(intValues, intValue)
		}
		if len(times) == 0 {
			times = intValues
		} else {
			distances = intValues
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	totalWays := 1
	for i := 0; i < len(times); i++ {
		ways := calculateWaysToWin(times[i], distances[i])
		totalWays *= ways
	}

	fmt.Println(totalWays)
}