package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	firewall := make(map[int]int)
	maxDepth := 0

	// Read input
	file, _ := os.Open("input.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), ": ")
		depth, _ := strconv.Atoi(parts[0])
		rang, _ := strconv.Atoi(parts[1])
		firewall[depth] = rang
		if depth > maxDepth {
			maxDepth = depth
		}
	}

	severity := 0
	for depth := 0; depth <= maxDepth; depth++ {
		if rang, exists := firewall[depth]; exists {
			if depth%(2*(rang-1)) == 0 {
				severity += depth * rang
			}
		}
	}

	fmt.Println(severity)
}
