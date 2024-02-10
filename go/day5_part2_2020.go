package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
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
	var seatIDs []int

	for scanner.Scan() {
		pass := scanner.Text()
		pass = strings.ReplaceAll(pass, "F", "0")
		pass = strings.ReplaceAll(pass, "B", "1")
		pass = strings.ReplaceAll(pass, "L", "0")
		pass = strings.ReplaceAll(pass, "R", "1")
		seatID := decode(pass)
		seatIDs = append(seatIDs, seatID)
	}

	sort.Ints(seatIDs)

	for i := 0; i < len(seatIDs)-1; i++ {
		// Check if the next seat ID is not the current seat ID + 1
		if seatIDs[i+1] != seatIDs[i]+1 {
			fmt.Println(seatIDs[i] + 1)
			break
		}
	}
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