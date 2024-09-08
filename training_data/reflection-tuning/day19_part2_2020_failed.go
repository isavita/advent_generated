package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Rule struct {
	char  byte
	subRules [][]int
}

func parseRules(input []string) map[int]Rule {
	rules := make(map[int]Rule)
	for _, line := range input {
		if line == "" {
			break
		}
		parts := strings.Split(line, ": ")
		id, _ := strconv.Atoi(parts[0])
		if strings.Contains(parts[1], "\"") {
			rules[id] = Rule{char: parts[1][1]}
		} else {
			var subRules [][]int
			for _, subRule := range strings.Split(parts[1], " | ") {
				var sub []int
				for _, num := range strings.Fields(subRule) {
					n, _ := strconv.Atoi(num)
					sub = append(sub, n)
				}
				subRules = append(subRules, sub)
			}
			rules[id] = Rule{subRules: subRules}
		}
	}
	return rules
}

func matchRule(msg string, pos int, ruleID int, rules map[int]Rule) []int {
	if pos >= len(msg) {
		return nil
	}
	rule := rules[ruleID]
	if rule.char != 0 {
		if msg[pos] == rule.char {
			return []int{pos + 1}
		}
		return nil
	}
	var matches []int
	for _, subRule := range rule.subRules {
		subMatches := []int{pos}
		for _, subRuleID := range subRule {
			var newMatches []int
			for _, p := range subMatches {
				newMatches = append(newMatches, matchRule(msg, p, subRuleID, rules)...)
			}
			subMatches = newMatches
			if len(subMatches) == 0 {
				break
			}
		}
		matches = append(matches, subMatches...)
	}
	return matches
}

func isValid(msg string, rules map[int]Rule) bool {
	matches := matchRule(msg, 0, 0, rules)
	for _, m := range matches {
		if m == len(msg) {
			return true
		}
	}
	return false
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	var input []string
	for scanner.Scan() {
		input = append(input, scanner.Text())
	}

	rules := parseRules(input)
	
	// Update rules for Part Two
	rules[8] = Rule{subRules: [][]int{{42}, {42, 8}}}
	rules[11] = Rule{subRules: [][]int{{42, 31}, {42, 11, 31}}}

	var messages []string
	for i, line := range input {
		if line == "" {
			messages = input[i+1:]
			break
		}
	}

	count := 0
	for _, msg := range messages {
		if isValid(msg, rules) {
			count++
		}
	}

	fmt.Println(count)
}
