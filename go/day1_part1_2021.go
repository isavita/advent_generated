package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	var prev, current, count int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		current, err = strconv.Atoi(scanner.Text())
		if err != nil {
			fmt.Println("Error reading number:", err)
			return
		}
		if prev != 0 && current > prev {
			count++
		}
		prev = current
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	fmt.Println(count)
}