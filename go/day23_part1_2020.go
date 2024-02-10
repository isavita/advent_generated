package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
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

	cups := make([]int, len(input)+1)
	var currentCup int
	for i, char := range input {
		cup, _ := strconv.Atoi(string(char))
		if i == 0 {
			currentCup = cup
		}
		if i < len(input)-1 {
			nextCup, _ := strconv.Atoi(string(input[i+1]))
			cups[cup] = nextCup
		}
	}
	firstCup, _ := strconv.Atoi(string(input[0]))
	lastCup, _ := strconv.Atoi(string(input[len(input)-1]))
	cups[lastCup] = firstCup

	for i := 0; i < 100; i++ {
		pickup1 := cups[currentCup]
		pickup2 := cups[pickup1]
		pickup3 := cups[pickup2]

		cups[currentCup] = cups[pickup3]

		destinationCup := currentCup - 1
		if destinationCup < 1 {
			destinationCup = len(input)
		}
		for destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3 {
			destinationCup--
			if destinationCup < 1 {
				destinationCup = len(input)
			}
		}

		cups[pickup3] = cups[destinationCup]
		cups[destinationCup] = pickup1

		currentCup = cups[currentCup]
	}

	cup := cups[1]
	for cup != 1 {
		fmt.Print(cup)
		cup = cups[cup]
		if cup == 1 {
			break
		}
	}
	fmt.Println()
}