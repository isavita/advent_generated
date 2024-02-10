package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

type Disc struct {
	totalPositions int
	startPosition  int
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	discs := []Disc{}
	scanner := bufio.NewScanner(file)
	discRegex := regexp.MustCompile(`Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).`)

	for scanner.Scan() {
		matches := discRegex.FindStringSubmatch(scanner.Text())
		totalPositions, _ := strconv.Atoi(matches[2])
		startPosition, _ := strconv.Atoi(matches[3])
		discs = append(discs, Disc{totalPositions, startPosition})
	}

	time := 0
	for {
		if checkDiscs(discs, time) {
			fmt.Println(time)
			break
		}
		time++
	}
}

func checkDiscs(discs []Disc, time int) bool {
	for i, disc := range discs {
		position := (disc.startPosition + time + i + 1) % disc.totalPositions
		if position != 0 {
			return false
		}
	}
	return true
}