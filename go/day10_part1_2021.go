package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	totalScore := 0
	for scanner.Scan() {
		line := scanner.Text()
		score, corrupted := checkLine(line)
		if corrupted {
			totalScore += score
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	fmt.Println(totalScore)
}

func checkLine(line string) (int, bool) {
	pairings := map[rune]rune{')': '(', ']': '[', '}': '{', '>': '<'}
	scores := map[rune]int{')': 3, ']': 57, '}': 1197, '>': 25137}
	stack := []rune{}

	for _, char := range line {
		switch char {
		case '(', '[', '{', '<':
			stack = append(stack, char)
		case ')', ']', '}', '>':
			if len(stack) == 0 || stack[len(stack)-1] != pairings[char] {
				return scores[char], true // corrupted line
			}
			stack = stack[:len(stack)-1] // pop from stack
		}
	}
	return 0, false // line is not corrupted
}