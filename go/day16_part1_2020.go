package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type Rule struct {
	name   string
	ranges [][2]int
}

func (r *Rule) isValid(value int) bool {
	for _, rng := range r.ranges {
		if value >= rng[0] && value <= rng[1] {
			return true
		}
	}
	return false
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	rules := []Rule{}
	scanningRules := true
	errorRate := 0

	reRule := regexp.MustCompile(`^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$`)

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		if strings.HasPrefix(line, "your ticket:") || strings.HasPrefix(line, "nearby tickets:") {
			scanningRules = false
			continue
		}
		if scanningRules {
			matches := reRule.FindStringSubmatch(line)
			if matches != nil {
				name := matches[1]
				range1 := [2]int{toInt(matches[2]), toInt(matches[3])}
				range2 := [2]int{toInt(matches[4]), toInt(matches[5])}
				rules = append(rules, Rule{name, [][2]int{range1, range2}})
			}
		} else {
			for _, value := range strings.Split(line, ",") {
				val := toInt(value)
				if !isValidForAnyRule(val, rules) {
					errorRate += val
				}
			}
		}
	}

	fmt.Println(errorRate)
}

func toInt(s string) int {
	val, _ := strconv.Atoi(s)
	return val
}

func isValidForAnyRule(value int, rules []Rule) bool {
	for _, rule := range rules {
		if rule.isValid(value) {
			return true
		}
	}
	return false
}