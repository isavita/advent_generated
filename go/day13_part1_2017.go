package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Scanner struct {
	Range     int
	Position  int
	Direction int
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}
	defer file.Close()

	firewall := make(map[int]*Scanner)
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		fields := strings.Split(scanner.Text(), ": ")
		depth, _ := strconv.Atoi(fields[0])
		rng, _ := strconv.Atoi(fields[1])
		firewall[depth] = &Scanner{Range: rng, Position: 0, Direction: 1}
	}

	severity := 0

	for depth := 0; depth <= maxDepth(firewall); depth++ {
		if scanner, ok := firewall[depth]; ok {
			if scanner.Position == 0 {
				severity += depth * scanner.Range
			}
		}

		for _, scanner := range firewall {
			moveScanner(scanner)
		}
	}

	fmt.Println(severity)
}

func maxDepth(firewall map[int]*Scanner) int {
	max := 0
	for depth := range firewall {
		if depth > max {
			max = depth
		}
	}
	return max
}

func moveScanner(scanner *Scanner) {
	if scanner.Position == 0 {
		scanner.Direction = 1
	} else if scanner.Position == scanner.Range-1 {
		scanner.Direction = -1
	}
	scanner.Position += scanner.Direction
}