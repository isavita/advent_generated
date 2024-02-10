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

	input := strings.TrimSpace(string(data))
	sum := 0

	for i := 0; i < len(input); i++ {
		next := (i + 1) % len(input)
		if input[i] == input[next] {
			sum += int(input[i] - '0')
		}
	}

	fmt.Println(sum)
}