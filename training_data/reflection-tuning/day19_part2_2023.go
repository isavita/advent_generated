package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Range struct {
	min, max int
}

type Workflow struct {
	rules []Rule
}

type Rule struct {
	category string
	op       string
	value    int
	dest     string
}

func main() {
	workflows := parseInput("input.txt")
	ranges := map[string]Range{
		"x": {1, 4000},
		"m": {1, 4000},
		"a": {1, 4000},
		"s": {1, 4000},
	}
	result := countAccepted(workflows, "in", ranges)
	fmt.Println(result)
}

func parseInput(filename string) map[string]Workflow {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)
	workflows := make(map[string]Workflow)

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			break
		}
		parts := strings.Split(line[:len(line)-1], "{")
		name := parts[0]
		ruleStrs := strings.Split(parts[1], ",")
		rules := make([]Rule, len(ruleStrs))
		for i, ruleStr := range ruleStrs {
			if strings.Contains(ruleStr, ":") {
				ruleParts := strings.Split(ruleStr, ":")
				rules[i] = Rule{
					category: string(ruleParts[0][0]),
					op:       string(ruleParts[0][1]),
					value:    mustAtoi(ruleParts[0][2:]),
					dest:     ruleParts[1],
				}
			} else {
				rules[i] = Rule{dest: ruleStr}
			}
		}
		workflows[name] = Workflow{rules}
	}
	return workflows
}

func countAccepted(workflows map[string]Workflow, current string, ranges map[string]Range) int64 {
	if current == "A" {
		return calculateCombinations(ranges)
	}
	if current == "R" {
		return 0
	}

	var total int64
	workflow := workflows[current]
	for _, rule := range workflow.rules {
		if rule.category == "" {
			total += countAccepted(workflows, rule.dest, ranges)
			break
		}

		trueRanges := copyRanges(ranges)
		falseRanges := copyRanges(ranges)

		r := ranges[rule.category]
		if rule.op == "<" {
			trueRanges[rule.category] = Range{r.min, min(r.max, rule.value-1)}
			falseRanges[rule.category] = Range{max(r.min, rule.value), r.max}
		} else {
			trueRanges[rule.category] = Range{max(r.min, rule.value+1), r.max}
			falseRanges[rule.category] = Range{r.min, min(r.max, rule.value)}
		}

		total += countAccepted(workflows, rule.dest, trueRanges)
		ranges = falseRanges
	}
	return total
}

func calculateCombinations(ranges map[string]Range) int64 {
	result := int64(1)
	for _, r := range ranges {
		result *= int64(r.max - r.min + 1)
	}
	return result
}

func copyRanges(ranges map[string]Range) map[string]Range {
	newRanges := make(map[string]Range)
	for k, v := range ranges {
		newRanges[k] = v
	}
	return newRanges
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func mustAtoi(s string) int {
	i, _ := strconv.Atoi(s)
	return i
}
