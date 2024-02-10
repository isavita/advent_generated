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

	correctedMessage := getCorrectedMessage(messages)
	fmt.Println(correctedMessage)
}

func getCorrectedMessage(messages []string) string {
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

	var correctedMessage strings.Builder
	for _, charCount := range count {
		correctedMessage.WriteRune(getMostCommonChar(charCount))
	}

	return correctedMessage.String()
}

func getMostCommonChar(count map[rune]int) rune {
	var maxChar rune
	maxCount := 0
	for char, cnt := range count {
		if cnt > maxCount {
			maxCount = cnt
			maxChar = char
		}
	}
	return maxChar
}