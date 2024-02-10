package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

const diskLength = 272 // Disk length for the problem

func main() {
	initialState := readInitialState("input.txt")
	data := generateData(initialState, diskLength)
	checksum := calculateChecksum(data)
	fmt.Println("Checksum:", checksum)
}

func readInitialState(filename string) string {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	if scanner.Scan() {
		return scanner.Text()
	}

	panic("Failed to read initial state")
}

func generateData(initialState string, length int) string {
	data := initialState
	for len(data) < length {
		var b strings.Builder
		for i := len(data) - 1; i >= 0; i-- {
			if data[i] == '0' {
				b.WriteRune('1')
			} else {
				b.WriteRune('0')
			}
		}
		data = data + "0" + b.String()
	}
	return data[:length]
}

func calculateChecksum(data string) string {
	for len(data)%2 == 0 {
		var b strings.Builder
		for i := 0; i < len(data); i += 2 {
			if data[i] == data[i+1] {
				b.WriteRune('1')
			} else {
				b.WriteRune('0')
			}
		}
		data = b.String()
	}
	return data
}