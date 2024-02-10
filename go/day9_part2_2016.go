package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan() // Assuming the input is a single line
	decompressedLength := getDecompressedLengthV2(scanner.Text())

	fmt.Println(decompressedLength)
}

func getDecompressedLengthV2(input string) int {
	return decompress(input, 0, len(input))
}

func decompress(input string, start, end int) int {
	markerRegex := regexp.MustCompile(`\((\d+)x(\d+)\)`)
	length := 0
	i := start
	for i < end {
		if marker := markerRegex.FindStringSubmatchIndex(input[i:end]); marker != nil {
			charCount, _ := strconv.Atoi(input[i+marker[2] : i+marker[3]])
			repeatCount, _ := strconv.Atoi(input[i+marker[4] : i+marker[5]])
			nextIndex := i + marker[1]
			length += repeatCount * decompress(input, nextIndex, nextIndex+charCount)
			i = nextIndex + charCount
		} else {
			length++
			i++
		}
	}
	return length
}