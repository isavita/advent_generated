package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func validatePassword(policy string, password string) bool {
	min, max, char := 0, 0, rune(0)
	fmt.Sscanf(policy, "%d-%d %c", &min, &max, &char)
	count := 0
	for _, c := range password {
		if c == char {
			count++
		}
	}
	return count >= min && count <= max
}

func main() {
	validCount := 0
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if i := strings.Index(line, ":"); i != -1 {
			policy, password := line[:i], line[i+2:]
			if validatePassword(policy, password) {
				validCount++
			}
		}
	}

	fmt.Println(validCount)
}