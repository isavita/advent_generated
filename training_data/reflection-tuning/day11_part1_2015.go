package main

import (
	"fmt"
	"strings"
)

func incrementPassword(password string) string {
	pw := []byte(password)
	for i := len(pw) - 1; i >= 0; i-- {
		if pw[i] == 'z' {
			pw[i] = 'a'
		} else {
			pw[i]++
			break
		}
	}
	return string(pw)
}

func hasIncreasingStraight(password string) bool {
	for i := 0; i < len(password)-2; i++ {
		if password[i+1] == password[i]+1 && password[i+2] == password[i]+2 {
			return true
		}
	}
	return false
}

func hasNoForbiddenLetters(password string) bool {
	return !strings.ContainsAny(password, "iol")
}

func hasTwoPairs(password string) bool {
	pairs := 0
	for i := 0; i < len(password)-1; i++ {
		if password[i] == password[i+1] {
			pairs++
			i++ // Skip next character to avoid overlapping
		}
	}
	return pairs >= 2
}

func isValidPassword(password string) bool {
	return hasIncreasingStraight(password) &&
		hasNoForbiddenLetters(password) &&
		hasTwoPairs(password)
}

func nextPassword(current string) string {
	next := incrementPassword(current)
	for !isValidPassword(next) {
		next = incrementPassword(next)
	}
	return next
}

func main() {
	input := "hxbxwxba" // Example input
	fmt.Println(nextPassword(input))
}
