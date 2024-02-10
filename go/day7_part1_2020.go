package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	contains := make(map[string][]string)
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), " bags contain ")
		container := parts[0]
		if parts[1] == "no other bags." {
			continue
		}
		containedBags := strings.Split(parts[1], ", ")
		for _, bag := range containedBags {
			bagName := strings.Join(strings.Fields(bag)[1:3], " ")
			contains[bagName] = append(contains[bagName], container)
		}
	}

	count := countCanContain("shiny gold", contains)
	fmt.Println(count)
}

func countCanContain(target string, contains map[string][]string) int {
	seen := make(map[string]bool)
	var dfs func(string)
	dfs = func(bag string) {
		for _, outer := range contains[bag] {
			if !seen[outer] {
				seen[outer] = true
				dfs(outer)
			}
		}
	}
	dfs(target)
	return len(seen)
}