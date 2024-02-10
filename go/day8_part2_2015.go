package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	totalDiff := 0
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		originalLength := len(line)
		encodedLength := calculateEncodedLength(line)
		totalDiff += encodedLength - originalLength
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	fmt.Println(totalDiff)
}

func calculateEncodedLength(s string) int {
	encoded := "\""
	for _, ch := range s {
		if ch == '\\' || ch == '"' {
			encoded += "\\"
		}
		encoded += string(ch)
	}
	encoded += "\""
	return len(encoded)
}