package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(file))
	floor := 0
	for _, c := range input {
		if c == '(' {
			floor++
		} else if c == ')' {
			floor--
		}
	}
	fmt.Println(floor)
}