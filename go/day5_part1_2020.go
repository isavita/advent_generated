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
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	maxSeatID := 0
	for scanner.Scan() {
		pass := scanner.Text()
		pass = strings.ReplaceAll(pass, "F", "0")
		pass = strings.ReplaceAll(pass, "B", "1")
		pass = strings.ReplaceAll(pass, "L", "0")
		pass = strings.ReplaceAll(pass, "R", "1")
		seatID := decode(pass)
		if seatID > maxSeatID {
			maxSeatID = seatID
		}
	}

	fmt.Println(maxSeatID)
}

func decode(pass string) int {
	row := binaryToInt(pass[:7])
	column := binaryToInt(pass[7:])
	return row*8 + column
}

func binaryToInt(binaryStr string) int {
	result := 0
	for i, char := range binaryStr {
		if char == '1' {
			result |= 1 << (len(binaryStr) - i - 1)
		}
	}
	return result
}