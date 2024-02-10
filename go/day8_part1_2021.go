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
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)

	count := 0
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " | ")
		_, output := parts[0], parts[1]
		for _, digit := range strings.Split(output, " ") {
			switch len(digit) {
			case 2, 4, 3, 7:
				count++
			}
		}
	}

	fmt.Println(count)
}