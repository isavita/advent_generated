package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func increment(password []byte) {
	for i := len(password) - 1; i >= 0; i-- {
		if password[i] == 'z' {
			password[i] = 'a'
		} else {
			password[i]++
			if password[i] == 'i' || password[i] == 'o' || password[i] == 'l' {
				password[i]++
			}
			break
		}
	}
}

func isValid(password []byte) bool {
	hasStraight := false
	pairs := make(map[byte]bool)
	pairCount := 0

	for i := 0; i < len(password); i++ {
		if password[i] == 'i' || password[i] == 'o' || password[i] == 'l' {
			return false
		}

		if i < len(password)-2 && password[i]+1 == password[i+1] && password[i+1]+1 == password[i+2] {
			hasStraight = true
		}

		if i < len(password)-1 && password[i] == password[i+1] {
			if !pairs[password[i]] {
				pairs[password[i]] = true
				pairCount++
			}
			i++ // Skip next character as it's part of the pair
		}
	}

	return hasStraight && pairCount >= 2
}

func nextPassword(current []byte) []byte {
	for {
		increment(current)
		if isValid(current) {
			return current
		}
	}
}

func main() {
	input, err := ioutil.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	password := []byte(strings.TrimSpace(string(input)))
	newPassword := nextPassword(password)

	fmt.Println(string(newPassword))
}
