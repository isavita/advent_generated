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
var maxLength int

func findStrongestLongestBridge(components []Component, used []bool, port, strength, length int) {
	if length > maxLength || (length == maxLength && strength > maxStrength) {
		maxStrength = strength
		maxLength = length
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
			findStrongestLongestBridge(components, used, nextPort, strength+c.a+c.b, length+1)
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
	findStrongestLongestBridge(components, used, 0, 0, 0)

	fmt.Println(maxStrength)
}