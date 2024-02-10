package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scores := []int{}
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if score, incomplete := checkAndCompleteLine(line); incomplete {
			scores = append(scores, score)
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	sort.Ints(scores)
	middleScore := scores[len(scores)/2]
	fmt.Println(middleScore)
}

func checkAndCompleteLine(line string) (int, bool) {
	pairings := map[rune]rune{')': '(', ']': '[', '}': '{', '>': '<'}
	scoreValues := map[rune]int{')': 1, ']': 2, '}': 3, '>': 4}
	opening := "([{<"
	closing := ")]}>"
	stack := []rune{}

	for _, char := range line {
		if strings.ContainsRune(opening, char) {
			stack = append(stack, char)
		} else if strings.ContainsRune(closing, char) {
			if len(stack) == 0 || stack[len(stack)-1] != pairings[char] {
				return 0, false // corrupted line
			}
			stack = stack[:len(stack)-1] // pop from stack
		}
	}

	if len(stack) == 0 {
		return 0, false // line is not incomplete
	}

	// Calculate score for incomplete line
	score := 0
	for i := len(stack) - 1; i >= 0; i-- {
		score *= 5
		score += scoreValues[getClosingChar(stack[i])]
	}
	return score, true
}

func getClosingChar(openingChar rune) rune {
	switch openingChar {
	case '(':
		return ')'
	case '[':
		return ']'
	case '{':
		return '}'
	case '<':
		return '>'
	default:
		return ' '
	}
}