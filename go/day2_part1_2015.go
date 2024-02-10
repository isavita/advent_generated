package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	total := 0
	for scanner.Scan() {
		dimensions := strings.Split(scanner.Text(), "x")
		if len(dimensions) != 3 {
			log.Fatal("Invalid input format")
		}

		l, _ := strconv.Atoi(dimensions[0])
		w, _ := strconv.Atoi(dimensions[1])
		h, _ := strconv.Atoi(dimensions[2])

		side1 := l * w
		side2 := w * h
		side3 := h * l

		smallest := min(side1, side2, side3)
		total += 2*side1 + 2*side2 + 2*side3 + smallest
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Println(total)
}

func min(vals ...int) int {
	minVal := vals[0]
	for _, val := range vals[1:] {
		if val < minVal {
			minVal = val
		}
	}
	return minVal
}