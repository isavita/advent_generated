package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		firstDigit, lastDigit := findFirstAndLastDigit(line)
		sum += 10*firstDigit + lastDigit
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	fmt.Println(sum)
}

func findFirstAndLastDigit(line string) (int, int) {
	digits := []string{"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}

	var firstDigit, lastDigit int
	for i, char := range line {
		digitStr := string(char)
		if digitStr >= "0" && digitStr <= "9" {
			if firstDigit == 0 {
				firstDigit = int(digitStr[0] - '0')
			}
			lastDigit = int(digitStr[0] - '0')
		} else {
			for j, digit := range digits {
				if strings.HasPrefix(line[i:], digit) {
					if firstDigit == 0 {
						firstDigit = j
					}
					lastDigit = j
					break
				}
			}
		}
	}

	return firstDigit, lastDigit
}