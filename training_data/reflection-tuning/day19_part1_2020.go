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

func main() {
	rules, messages := parseInput("input.txt")
	count := countMatchingMessages(rules, messages)
	fmt.Println(count)
}

func parseInput(filename string) (map[int]Rule, []string) {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)

	rules := make(map[int]Rule)
	var messages []string
	parsingRules := true

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			parsingRules = false
			continue
		}

		if parsingRules {
			parts := strings.Split(line, ": ")
			id, _ := strconv.Atoi(parts[0])
			if strings.Contains(parts[1], "\"") {
				rules[id] = Rule{char: parts[1][1]}
			} else {
				var subRules [][]int
				for _, subRule := range strings.Split(parts[1], " | ") {
					var nums []int
					for _, num := range strings.Fields(subRule) {
						n, _ := strconv.Atoi(num)
						nums = append(nums, n)
					}
					subRules = append(subRules, nums)
				}
				rules[id] = Rule{subRules: subRules}
			}
		} else {
			messages = append(messages, line)
		}
	}

	return rules, messages
}

func countMatchingMessages(rules map[int]Rule, messages []string) int {
	count := 0
	for _, msg := range messages {
		if matchRule(rules, 0, msg, 0) == len(msg) {
			count++
		}
	}
	return count
}

func matchRule(rules map[int]Rule, ruleID int, msg string, pos int) int {
	if pos >= len(msg) {
		return -1
	}

	rule := rules[ruleID]

	if rule.char != 0 {
		if msg[pos] == rule.char {
			return pos + 1
		}
		return -1
	}

	for _, subRule := range rule.subRules {
		newPos := pos
		match := true
		for _, subRuleID := range subRule {
			newPos = matchRule(rules, subRuleID, msg, newPos)
			if newPos == -1 {
				match = false
				break
			}
		}
		if match {
			return newPos
		}
	}

	return -1
}
