package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Component struct {
	a, b int
}

var maxStrength int

func findStrongestBridge(components []Component, used []bool, port, strength int) {
	if strength > maxStrength {
		maxStrength = strength
	}

	for i, c := range components {
		if used[i] {
			continue
		}

		if c.a == port || c.b == port {
			used[i] = true
			nextPort := c.a
			if c.a == port {
				nextPort = c.b
			}
			findStrongestBridge(components, used, nextPort, strength+c.a+c.b)
			used[i] = false
		}
	}
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var components []Component
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		ports := strings.Split(scanner.Text(), "/")
		a, _ := strconv.Atoi(ports[0])
		b, _ := strconv.Atoi(ports[1])
		components = append(components, Component{a, b})
	}

	used := make([]bool, len(components))
	findStrongestBridge(components, used, 0, 0)

	fmt.Println(maxStrength)
}