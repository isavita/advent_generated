package main

import (
	"fmt"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading from file:", err)
	}
	input := strings.TrimSpace(string(file))

	fmt.Println(someAssemblyRequired(input))
}

func someAssemblyRequired(input string) int {
	wireToRule := map[string]string{}

	for _, inst := range strings.Split(input, "\n") {
		parts := strings.Split(inst, " -> ")
		wireToRule[parts[1]] = parts[0]
	}

	aSignal := memoDFS(wireToRule, "a", map[string]int{})
	return aSignal
}

func memoDFS(graph map[string]string, entry string, memo map[string]int) int {
	if memoVal, ok := memo[entry]; ok {
		return memoVal
	}

	// if it's a number, return the casted value
	if regexp.MustCompile("[0-9]").MatchString(entry) {
		return toInt(entry)
	}

	sourceRule := graph[entry]
	parts := strings.Split(sourceRule, " ")

	var result int
	switch {
	case len(parts) == 1:
		result = memoDFS(graph, parts[0], memo)
	case parts[0] == "NOT":
		start := memoDFS(graph, parts[1], memo)
		result = (math.MaxUint16) ^ start
	case parts[1] == "AND":
		result = memoDFS(graph, parts[0], memo) & memoDFS(graph, parts[2], memo)
	case parts[1] == "OR":
		result = memoDFS(graph, parts[0], memo) | memoDFS(graph, parts[2], memo)
	case parts[1] == "LSHIFT":
		result = memoDFS(graph, parts[0], memo) << memoDFS(graph, parts[2], memo)
	case parts[1] == "RSHIFT":
		result = memoDFS(graph, parts[0], memo) >> memoDFS(graph, parts[2], memo)
	}

	memo[entry] = result
	return result
}

func toInt(s string) int {
	n, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return n
}