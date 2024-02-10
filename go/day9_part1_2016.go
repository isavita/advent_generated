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
	decompressedLength := getDecompressedLength(scanner.Text())

	fmt.Println(decompressedLength)
}

func getDecompressedLength(input string) int {
	markerRegex := regexp.MustCompile(`\((\d+)x(\d+)\)`)
	length := 0
	for i := 0; i < len(input); {
		if marker := markerRegex.FindStringSubmatchIndex(input[i:]); marker != nil {
			charCount, _ := strconv.Atoi(input[i+marker[2] : i+marker[3]])
			repeatCount, _ := strconv.Atoi(input[i+marker[4] : i+marker[5]])
			length += charCount * repeatCount
			i += marker[1] + charCount
		} else {
			length++
			i++
		}
	}
	return length
}