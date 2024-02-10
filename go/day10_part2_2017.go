package main

import (
	"bufio"
	"encoding/hex"
	"fmt"
	"os"
)

func main() {
	// Read input from a file
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	input := scanner.Text()

	// Convert input to ASCII codes
	var lengths []int
	for i := 0; i < len(input); i++ {
		lengths = append(lengths, int(input[i]))
	}
	lengths = append(lengths, 17, 31, 73, 47, 23)

	// Initialize variables
	list := make([]int, 256)
	for i := 0; i < 256; i++ {
		list[i] = i
	}
	currentPosition := 0
	skipSize := 0

	// Perform 64 rounds
	for round := 0; round < 64; round++ {
		for _, length := range lengths {
			for i := 0; i < length/2; i++ {
				start := (currentPosition + i) % 256
				end := (currentPosition + length - 1 - i) % 256
				list[start], list[end] = list[end], list[start]
			}
			currentPosition = (currentPosition + length + skipSize) % 256
			skipSize++
		}
	}

	// Calculate the dense hash
	var denseHash []byte
	for i := 0; i < 256; i += 16 {
		xor := 0
		for j := 0; j < 16; j++ {
			xor ^= list[i+j]
		}
		denseHash = append(denseHash, byte(xor))
	}

	// Convert to hexadecimal
	hexHash := hex.EncodeToString(denseHash)

	fmt.Println(hexHash)
}