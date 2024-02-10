package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func transform(subjectNumber, loopSize int) int {
	value := 1
	for i := 0; i < loopSize; i++ {
		value *= subjectNumber
		value %= 20201227
	}
	return value
}

func findLoopSize(publicKey int) int {
	value := 1
	loopSize := 0
	for value != publicKey {
		value *= 7
		value %= 20201227
		loopSize++
	}
	return loopSize
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	cardPublicKey, _ := strconv.Atoi(scanner.Text())
	scanner.Scan()
	doorPublicKey, _ := strconv.Atoi(scanner.Text())

	cardLoopSize := findLoopSize(cardPublicKey)
	encryptionKey := transform(doorPublicKey, cardLoopSize)

	fmt.Println(encryptionKey)
}