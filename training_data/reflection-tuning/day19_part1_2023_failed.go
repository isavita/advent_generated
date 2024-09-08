package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Part struct {
	x, m, a, s int
}

type Rule struct {
	category    string
	operator    string
	value       int
	destination string
}

type Workflow struct {
	name  string
	rules []Rule
}

func parseWorkflows(input []string) map[string]Workflow {
	workflows := make(map[string]Workflow)
	for _, line := range input {
		if line == "" {
			break
		}
		parts := strings.Split(line, "{")
		name := parts[0]
		rulesStr := strings.Trim(parts[1], "}")
		rulesParts := strings.Split(rulesStr, ",")
		rules := make([]Rule, 0)
		for _, rulePart := range rulesParts {
			if strings.Contains(rulePart, ":") {
				ruleSplit := strings.Split(rulePart, ":")
				condition := ruleSplit[0]
				destination := ruleSplit[1]
				category := string(condition[0])
				operator := string(condition[1])
				value, _ := strconv.Atoi(condition[2:])
				rules = append(rules, Rule{category, operator, value, destination})
			} else {
				rules = append(rules, Rule{"", "", 0, rulePart})
			}
		}
		workflows[name] = Workflow{name, rules}
	}
	return workflows
}

func parseParts(input []string) []Part {
	var parts []Part
	for _, line := range input {
		if !strings.HasPrefix(line, "{") {
			continue
		}
		line = strings.Trim(line, "{}")
		values := strings.Split(line, ",")
		part := Part{}
		for _, v := range values {
			kv := strings.Split(v, "=")
			value, _ := strconv.Atoi(kv[1])
			switch kv[0] {
			case "x":
				part.x = value
			case "m":
				part.m = value
			case "a":
				part.a = value
			case "s":
				part.s = value
			}
		}
		parts = append(parts, part)
	}
	return parts
}

func processPart(part Part, workflows map[string]Workflow) bool {
	currentWorkflow := "in"
	for {
		workflow := workflows[currentWorkflow]
		for _, rule := range workflow.rules {
			if rule.category == "" {
				if rule.destination == "A" {
					return true
				}
				if rule.destination == "R" {
					return false
				}
				currentWorkflow = rule.destination
				break
			}
			value := 0
			switch rule.category {
			case "x":
				value = part.x
			case "m":
				value = part.m
			case "a":
				value = part.a
			case "s":
				value = part.s
			}
			if (rule.operator == ">" && value > rule.value) || (rule.operator == "<" && value < rule.value) {
				if rule.destination == "A" {
					return true
				}
				if rule.destination == "R" {
					return false
				}
				currentWorkflow = rule.destination
				break
			}
		}
	}
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	var input []string
	for scanner.Scan() {
		input = append(input, scanner.Text())
	}

	workflows := parseWorkflows(input)
	parts := parseParts(input)

	sum := 0
	for _, part := range parts {
		if processPart(part, workflows) {
			sum += part.x + part.m + part.a + part.s
		}
	}

	fmt.Println(sum)
}
