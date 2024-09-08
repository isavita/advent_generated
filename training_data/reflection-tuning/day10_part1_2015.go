package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func lookAndSay(s string) string {
	var result strings.Builder
	count := 1
	current := s[0]

	for i := 1; i < len(s); i++ {
		if s[i] == current {
			count++
		} else {
			result.WriteString(fmt.Sprintf("%d%c", count, current))
			count = 1
			current = s[i]
		}
	}
	result.WriteString(fmt.Sprintf("%d%c", count, current))

	return result.String()
}

func main() {
	input, err := ioutil.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}

	sequence := strings.TrimSpace(string(input))

	for i := 0; i < 40; i++ {
		sequence = lookAndSay(sequence)
	}

	fmt.Println(len(sequence))
}
