package main

import (
	"fmt"
	"os"
	"strings"
)

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
			if wordSet[word] {
				valid = false
				break
			}
			wordSet[word] = true
		}

		if valid {
			validCount++
		}
	}

	fmt.Println(validCount)
}