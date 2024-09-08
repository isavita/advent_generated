package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	firewall := parseInput("input.txt")
	maxDepth := getMaxDepth(firewall)

	// Part 1
	severity := calculateSeverity(firewall, maxDepth, 0)
	fmt.Println("Part 1:", severity)

	// Part 2
	delay := findSafeDelay(firewall, maxDepth)
	fmt.Println("Part 2:", delay)
}

func parseInput(filename string) map[int]int {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	firewall := make(map[int]int)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), ": ")
		depth, _ := strconv.Atoi(parts[0])
		rang, _ := strconv.Atoi(parts[1])
		firewall[depth] = rang
	}
	return firewall
}

func getMaxDepth(firewall map[int]int) int {
	maxDepth := 0
	for depth := range firewall {
		if depth > maxDepth {
			maxDepth = depth
		}
	}
	return maxDepth
}

func calculateSeverity(firewall map[int]int, maxDepth, delay int) int {
	severity := 0
	for depth := 0; depth <= maxDepth; depth++ {
		if rang, exists := firewall[depth]; exists {
			if (depth+delay)%(2*(rang-1)) == 0 {
				severity += depth * rang
			}
		}
	}
	return severity
}

func isCaught(firewall map[int]int, maxDepth, delay int) bool {
	for depth := 0; depth <= maxDepth; depth++ {
		if rang, exists := firewall[depth]; exists {
			if (depth+delay)%(2*(rang-1)) == 0 {
				return true
			}
		}
	}
	return false
}

func findSafeDelay(firewall map[int]int, maxDepth int) int {
	delay := 0
	for isCaught(firewall, maxDepth, delay) {
		delay++
	}
	return delay
}
