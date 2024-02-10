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

	delay := 0
	for {
		if passThrough(firewall, delay) {
			break
		}
		delay++
	}

	fmt.Println(delay)
}

func passThrough(firewall map[int]*Scanner, delay int) bool {
	for depth, scanner := range firewall {
		if (depth+delay)%(2*(scanner.Range-1)) == 0 {
			return false
		}
	}
	return true
}