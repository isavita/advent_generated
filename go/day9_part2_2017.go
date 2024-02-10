package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	// Step 1: Read Input
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}
	defer file.Close()

	// Step 2: Initialize Variables
	score := 0
	depth := 0
	inGarbage := false
	cancelNext := false
	garbageCount := 0

	// Step 3: Process Stream
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		for _, ch := range scanner.Text() {
			if cancelNext {
				cancelNext = false
				continue
			}

			if inGarbage {
				if ch == '!' {
					cancelNext = true
				} else if ch == '>' {
					inGarbage = false
				} else {
					garbageCount++
				}
			} else {
				switch ch {
				case '{':
					depth++
				case '}':
					score += depth
					depth--
				case '<':
					inGarbage = true
				}
			}
		}
	}

	// Step 4: Print Results
	fmt.Println(garbageCount)
}