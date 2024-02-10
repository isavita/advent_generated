package main

import (
	"encoding/hex"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func reverseSection(arr []int, start, length int) {
	n := len(arr)
	for i, j := start, start+length-1; i < j; i, j = i+1, j-1 {
		arr[i%n], arr[j%n] = arr[j%n], arr[i%n]
	}
}

func knotHash(input string) string {
	lengths := []int{}
	for _, char := range input {
		lengths = append(lengths, int(char))
	}
	lengths = append(lengths, 17, 31, 73, 47, 23)

	list := make([]int, 256)
	for i := range list {
		list[i] = i
	}

	var position, skip int
	for round := 0; round < 64; round++ {
		for _, length := range lengths {
			reverseSection(list, position, length)
			position += length + skip
			skip++
		}
	}

	// Dense hash calculation
	denseHash := make([]int, 16)
	for i := 0; i < 16; i++ {
		xor := 0
		for j := 0; j < 16; j++ {
			xor ^= list[i*16+j]
		}
		denseHash[i] = xor
	}

	// Convert to hexadecimal
	hexHash := make([]byte, 16)
	for i, v := range denseHash {
		hexHash[i] = byte(v)
	}
	return hex.EncodeToString(hexHash)
}

func hexToBinary(hexStr string) string {
	binaryStr := ""
	for _, hexDigit := range hexStr {
		val, _ := strconv.ParseUint(string(hexDigit), 16, 64)
		binaryStr += fmt.Sprintf("%.4b", val)
	}
	return binaryStr
}

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}

	keyString := strings.TrimSpace(string(data))
	totalUsed := 0

	for i := 0; i < 128; i++ {
		rowKey := fmt.Sprintf("%s-%d", keyString, i)
		hash := knotHash(rowKey)
		binaryRow := hexToBinary(hash)

		for _, bit := range binaryRow {
			if bit == '1' {
				totalUsed++
			}
		}
	}

	fmt.Println(totalUsed)
}