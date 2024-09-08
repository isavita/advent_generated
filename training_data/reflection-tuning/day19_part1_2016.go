package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	// Read input from file
	content, err := ioutil.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	// Convert input to integer
	n, err := strconv.Atoi(strings.TrimSpace(string(content)))
	if err != nil {
		fmt.Println("Error converting input to integer:", err)
		return
	}

	// Find the largest power of 2 less than or equal to n
	k := 0
	for 1<<k <= n {
		k++
	}
	k--

	// Calculate the winning position
	l := n - (1 << k)
	winningPosition := 2*l + 1

	// Print the result
	fmt.Println(winningPosition)
}
