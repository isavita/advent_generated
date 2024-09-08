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
			break
		}
	}
}

func hasStraight(password string) bool {
	for i := 0; i < len(password)-2; i++ {
		if password[i+1] == password[i]+1 && password[i+2] == password[i]+2 {
			return true
		}
	}
	return false
}

func hasNoIOL(password string) bool {
	return !strings.ContainsAny(password, "iol")
}

func hasTwoPairs(password string) bool {
	pairs := 0
	for i := 0; i < len(password)-1; i++ {
		if password[i] == password[i+1] {
			pairs++
			i++
		}
	}
	return pairs >= 2
}

func isValid(password string) bool {
	return hasStraight(password) && hasNoIOL(password) && hasTwoPairs(password)
}

func nextPassword(current string) string {
	password := []byte(current)
	for {
		increment(password)
		if isValid(string(password)) {
			return string(password)
		}
	}
}

func main() {
	input, _ := ioutil.ReadFile("input.txt")
	currentPassword := strings.TrimSpace(string(input))

	nextPwd := nextPassword(currentPassword)
	fmt.Println("Part 1:", nextPwd)

	nextNextPwd := nextPassword(nextPwd)
	fmt.Println("Part 2:", nextNextPwd)
}
