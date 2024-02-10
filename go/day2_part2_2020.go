package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func validatePassword(policy string, password string) bool {
	min, max := 0, 0
	char := rune(0)
	fmt.Sscanf(policy, "%d-%d %c", &min, &max, &char)
	return (password[min-1] == byte(char)) != (password[max-1] == byte(char))
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