package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var messages []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		messages = append(messages, scanner.Text())
	}

	originalMessage := getOriginalMessage(messages)
	fmt.Println(originalMessage)
}

func getOriginalMessage(messages []string) string {
	if len(messages) == 0 {
		return ""
	}
	messageLength := len(messages[0])
	count := make([]map[rune]int, messageLength)

	for i := range count {
		count[i] = make(map[rune]int)
	}

	for _, message := range messages {
		for j, char := range message {
			count[j][char]++
		}
	}

	var originalMessage strings.Builder
	for _, charCount := range count {
		originalMessage.WriteRune(getLeastCommonChar(charCount))
	}

	return originalMessage.String()
}

func getLeastCommonChar(count map[rune]int) rune {
	var minChar rune
	minCount := int(^uint(0) >> 1) // maximum int value
	for char, cnt := range count {
		if cnt < minCount {
			minCount = cnt
			minChar = char
		}
	}
	return minChar
}