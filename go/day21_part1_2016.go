package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	// Read the input from input.txt
	input, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading input file:", err)
		return
	}

	// Convert the input bytes to string and split it into individual operations
	operations := strings.Split(strings.TrimSpace(string(input)), "\n")

	// Initial password
	password := "abcdefgh"

	// Apply the operations to the password
	for _, op := range operations {
		password = applyOperation(op, password)
	}

	// Print the result
	fmt.Println(password)
}

func applyOperation(op string, password string) string {
	fields := strings.Fields(op)
	switch fields[0] {
	case "swap":
		switch fields[1] {
		case "position":
			x, y := int(fields[2][0]-'0'), int(fields[5][0]-'0')
			password = swapPosition(password, x, y)
		case "letter":
			x, y := fields[2][0], fields[5][0]
			password = swapLetter(password, x, y)
		}
	case "rotate":
		switch fields[1] {
		case "left":
			steps := int(fields[2][0] - '0')
			password = rotateLeft(password, steps)
		case "right":
			steps := int(fields[2][0] - '0')
			password = rotateRight(password, steps)
		case "based":
			x := fields[6][0]
			password = rotateBasedOnPosition(password, x)
		}
	case "reverse":
		x, y := int(fields[2][0]-'0'), int(fields[4][0]-'0')
		password = reversePositions(password, x, y)
	case "move":
		x, y := int(fields[2][0]-'0'), int(fields[5][0]-'0')
		password = movePosition(password, x, y)
	}
	return password
}

func swapPosition(password string, x, y int) string {
	runes := []rune(password)
	runes[x], runes[y] = runes[y], runes[x]
	return string(runes)
}

func swapLetter(password string, x, y byte) string {
	runes := []rune(password)
	for i, r := range runes {
		if r == rune(x) {
			runes[i] = rune(y)
		} else if r == rune(y) {
			runes[i] = rune(x)
		}
	}
	return string(runes)
}

func rotateLeft(password string, steps int) string {
	steps = steps % len(password)
	return password[steps:] + password[:steps]
}

func rotateRight(password string, steps int) string {
	steps = steps % len(password)
	return password[len(password)-steps:] + password[:len(password)-steps]
}

func rotateBasedOnPosition(password string, x byte) string {
	index := strings.IndexByte(password, x)
	steps := 1 + index
	if index >= 4 {
		steps++
	}
	return rotateRight(password, steps)
}

func reversePositions(password string, x, y int) string {
	runes := []rune(password)
	for i, j := x, y; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func movePosition(password string, x, y int) string {
	runes := []rune(password)
	r := runes[x]
	runes = append(runes[:x], runes[x+1:]...)
	runes = append(runes[:y], append([]rune{r}, runes[y:]...)...)
	return string(runes)
}