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
		codeLength := len(line)
		memoryLength := calculateMemoryLength(line)
		totalDiff += codeLength - memoryLength
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	fmt.Println(totalDiff)
}

func calculateMemoryLength(s string) int {
	length := 0
	inEscape := false
	hexCount := 0

	for i := 1; i < len(s)-1; i++ {
		switch {
		case hexCount > 0:
			hexCount--
		case inEscape:
			if s[i] == 'x' {
				hexCount = 2
			}
			inEscape = false
			length++
		case s[i] == '\\':
			inEscape = true
		default:
			length++
		}
	}
	return length
}