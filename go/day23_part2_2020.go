package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

const (
	totalCups  = 1000000
	totalMoves = 10000000
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	input := scanner.Text()

	cups := make([]int, totalCups+1)
	var lastCup int

	for i, char := range input {
		cup, _ := strconv.Atoi(string(char))
		if i > 0 {
			cups[lastCup] = cup
		}
		lastCup = cup
	}

	for i := len(input) + 1; i <= totalCups; i++ {
		cups[lastCup] = i
		lastCup = i
	}
	cups[lastCup], _ = strconv.Atoi(string(input[0]))

	currentCup, _ := strconv.Atoi(string(input[0]))
	for i := 0; i < totalMoves; i++ {
		pickup1 := cups[currentCup]
		pickup2 := cups[pickup1]
		pickup3 := cups[pickup2]

		cups[currentCup] = cups[pickup3]

		destinationCup := currentCup - 1
		if destinationCup == 0 {
			destinationCup = totalCups
		}
		for destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3 {
			destinationCup--
			if destinationCup == 0 {
				destinationCup = totalCups
			}
		}

		cups[pickup3] = cups[destinationCup]
		cups[destinationCup] = pickup1

		currentCup = cups[currentCup]
	}

	cup1 := cups[1]
	cup2 := cups[cup1]
	fmt.Println(cup1 * cup2)
}