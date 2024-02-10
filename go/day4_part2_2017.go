package main

import (
	"fmt"
	"os"
	"sort"
	"strings"
)

func sortString(w string) string {
	s := strings.Split(w, "")
	sort.Strings(s)
	return strings.Join(s, "")
}

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}

	passphrases := strings.Split(strings.TrimSpace(string(data)), "\n")
	validCount := 0

	for _, passphrase := range passphrases {
		words := strings.Fields(passphrase)
		wordSet := make(map[string]bool)

		valid := true
		for _, word := range words {
			sortedWord := sortString(word)
			if wordSet[sortedWord] {
				valid = false
				break
			}
			wordSet[sortedWord] = true
		}

		if valid {
			validCount++
		}
	}

	fmt.Println(validCount)
}