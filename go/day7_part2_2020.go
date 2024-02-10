package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type BagRule struct {
	Color string
	Count int
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	rules := make(map[string][]BagRule)
	scanner := bufio.NewScanner(file)
	ruleRegex := regexp.MustCompile(`(\d+) (\w+ \w+) bags?[,.]`)

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " bags contain ")
		container := parts[0]
		contents := parts[1]

		if contents == "no other bags." {
			continue
		}

		for _, match := range ruleRegex.FindAllStringSubmatch(contents, -1) {
			count, _ := strconv.Atoi(match[1])
			rules[container] = append(rules[container], BagRule{Color: match[2], Count: count})
		}
	}

	totalBags := countBags("shiny gold", rules) - 1
	fmt.Println(totalBags)
}

func countBags(color string, rules map[string][]BagRule) int {
	count := 1
	for _, rule := range rules[color] {
		count += rule.Count * countBags(rule.Color, rules)
	}
	return count
}