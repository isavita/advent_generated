package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
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
	totalRibbon := 0
	for scanner.Scan() {
		dimensions := strings.Split(scanner.Text(), "x")
		if len(dimensions) != 3 {
			log.Fatal("Invalid input format")
		}

		l, _ := strconv.Atoi(dimensions[0])
		w, _ := strconv.Atoi(dimensions[1])
		h, _ := strconv.Atoi(dimensions[2])

		// Calculate ribbon for the bow
		bow := l * w * h

		// Calculate ribbon for wrapping (smallest perimeter)
		sides := []int{l, w, h}
		sort.Ints(sides)
		wrap := 2*sides[0] + 2*sides[1]

		totalRibbon += bow + wrap
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Println(totalRibbon)
}