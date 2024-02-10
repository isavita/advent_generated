package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	tlsCount := 0
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		if supportsTLS(scanner.Text()) {
			tlsCount++
		}
	}

	fmt.Println(tlsCount)
}

func supportsTLS(ip string) bool {
	insideBrackets := regexp.MustCompile(`\[[a-z]+\]`)
	bracketContents := insideBrackets.FindAllString(ip, -1)

	for _, bracketContent := range bracketContents {
		if containsABBA(bracketContent) {
			return false
		}
	}

	ip = insideBrackets.ReplaceAllString(ip, "-")
	return containsABBA(ip)
}

func containsABBA(s string) bool {
	for i := 0; i < len(s)-3; i++ {
		if s[i] != s[i+1] && s[i] == s[i+3] && s[i+1] == s[i+2] {
			return true
		}
	}
	return false
}