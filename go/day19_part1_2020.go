package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// Reads the rules and constructs a map.
func readRules(scanner *bufio.Scanner) map[int]string {
	rules := make(map[int]string)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			break
		}
		parts := strings.Split(line, ": ")
		rules[atoi(parts[0])] = strings.Replace(parts[1], "\"", "", -1)
	}
	return rules
}

// Converts string to int with error handling.
func atoi(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return i
}

// Constructs a regex pattern from the rules.
func constructPattern(rules map[int]string, index int) string {
	if strings.Contains(rules[index], "|") {
		subrules := strings.Split(rules[index], " | ")
		parts := []string{}
		for _, subrule := range subrules {
			parts = append(parts, constructSubPattern(rules, subrule))
		}
		return "(" + strings.Join(parts, "|") + ")"
	}
	return constructSubPattern(rules, rules[index])
}

// Constructs a sub-pattern for part of a rule.
func constructSubPattern(rules map[int]string, subrule string) string {
	if subrule == "a" || subrule == "b" {
		return subrule
	}
	subIdxs := strings.Split(subrule, " ")
	pattern := ""
	for _, idx := range subIdxs {
		pattern += constructPattern(rules, atoi(idx))
	}
	return pattern
}

// Counts the number of messages that match rule 0.
func countMatches(scanner *bufio.Scanner, pattern string) int {
	count := 0
	re := regexp.MustCompile("^" + pattern + "$")
	for scanner.Scan() {
		message := scanner.Text()
		if re.MatchString(message) {
			count++
		}
	}
	return count
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	rules := readRules(scanner)
	pattern := constructPattern(rules, 0)
	count := countMatches(scanner, pattern)

	fmt.Println("The number of messages that completely match rule 0 is:", count)
}