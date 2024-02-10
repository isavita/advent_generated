package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	currentPassword, err := readInput("input.txt")
	if err != nil {
		fmt.Println("Error reading input:", err)
		return
	}

	newPassword := findNextPassword(currentPassword)
	fmt.Println(newPassword)
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

func findNextPassword(password string) string {
	for {
		password = incrementPassword(password)
		if isValidPassword(password) {
			break
		}
	}
	return password
}

func incrementPassword(password string) string {
	runes := []rune(password)
	for i := len(runes) - 1; i >= 0; i-- {
		runes[i]++
		if runes[i] > 'z' {
			runes[i] = 'a'
		} else {
			break
		}
	}
	return string(runes)
}

func isValidPassword(password string) bool {
	return hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password)
}

func hasStraight(password string) bool {
	for i := 0; i < len(password)-2; i++ {
		if password[i]+1 == password[i+1] && password[i]+2 == password[i+2] {
			return true
		}
	}
	return false
}

func containsInvalidLetters(password string) bool {
	for _, c := range password {
		if c == 'i' || c == 'o' || c == 'l' {
			return true
		}
	}
	return false
}

func hasTwoPairs(password string) bool {
	count := 0
	for i := 0; i < len(password)-1; i++ {
		if password[i] == password[i+1] {
			count++
			i++ // Skip the next character
		}
	}
	return count >= 2
}