package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	var fishes [9]int
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanWords)
	scanner.Scan()
	input := scanner.Text()
	fishStrs := strings.Split(input, ",")
	for _, fishStr := range fishStrs {
		fish, _ := strconv.Atoi(fishStr)
		fishes[fish]++
	}

	for day := 1; day <= 80; day++ {
		newFish := fishes[0]
		for i := 1; i < len(fishes); i++ {
			fishes[i-1] = fishes[i]
		}
		fishes[6] += newFish
		fishes[8] = newFish
	}

	totalFish := 0
	for _, fish := range fishes {
		totalFish += fish
	}

	fmt.Println(totalFish)
}