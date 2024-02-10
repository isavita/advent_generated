package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func calculateWaysToWinLongRace(time, record int) int {
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
	var time, distance int

	// Reading time and distance as single long numbers
	for scanner.Scan() {
		text := scanner.Text()
		if text == "" {
			continue
		}
		parts := strings.Split(text, ":")
		line := strings.Replace(parts[1], " ", "", -1) // Remove spaces
		if time == 0 {
			time, _ = strconv.Atoi(line)
		} else {
			distance, _ = strconv.Atoi(line)
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	waysToWin := calculateWaysToWinLongRace(time, distance)

	fmt.Println(waysToWin)
}