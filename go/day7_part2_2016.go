package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	sslCount := 0
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		if supportsSSL(scanner.Text()) {
			sslCount++
		}
	}

	fmt.Println(sslCount)
}

func supportsSSL(ip string) bool {
	insideBrackets := regexp.MustCompile(`\[[a-z]+\]`)
	bracketContents := insideBrackets.FindAllString(ip, -1)

	// Generate BABs from ABAs found outside brackets
	ip = insideBrackets.ReplaceAllString(ip, "-")
	for _, aba := range findABAs(ip) {
		bab := string([]rune{rune(aba[1]), rune(aba[0]), rune(aba[1])})
		for _, bracketContent := range bracketContents {
			if strings.Contains(bracketContent, bab) {
				return true
			}
		}
	}

	return false
}

func findABAs(s string) []string {
	var abas []string
	for i := 0; i < len(s)-2; i++ {
		if s[i] != s[i+1] && s[i] == s[i+2] {
			abas = append(abas, s[i:i+3])
		}
	}
	return abas
}