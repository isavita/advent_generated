package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	initialSequence, err := readInput("input.txt")
	if err != nil {
		fmt.Println("Error reading input:", err)
		return
	}

	result := lookAndSay(initialSequence, 50)
	fmt.Println(len(result))
}

func readInput(filename string) (string, error) {
	file, err := os.Open(filename)
	if err != nil {
		return "", err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	if scanner.Scan() {
		return scanner.Text(), nil
	}
	return "", scanner.Err()
}

func lookAndSay(sequence string, iterations int) string {
	for i := 0; i < iterations; i++ {
		sequence = nextSequence(sequence)
	}
	return sequence
}

func nextSequence(sequence string) string {
	var result strings.Builder
	for i := 0; i < len(sequence); {
		count, digit := 1, sequence[i]
		for j := i + 1; j < len(sequence) && sequence[j] == digit; j++ {
			count++
		}
		result.WriteString(fmt.Sprintf("%d%c", count, digit))
		i += count
	}
	return result.String()
}