package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	counts := make([][2]int, 12)
	for scanner.Scan() {
		num := scanner.Text()
		for i := 0; i < len(num); i++ {
			counts[i][num[i]-'0']++
		}
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	gammaRate := 0
	epsilonRate := 0
	for i := 0; i < len(counts); i++ {
		if counts[i][0] > counts[i][1] {
			gammaRate |= 1 << (len(counts) - i - 1)
		} else {
			epsilonRate |= 1 << (len(counts) - i - 1)
		}
	}

	fmt.Println(gammaRate * epsilonRate)
}