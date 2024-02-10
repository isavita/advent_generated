package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Rule struct {
	Category     byte
	Operator     byte
	Num          int
	WorkflowName string
}

type Workflows map[string][]Rule

type Part map[byte]int

type Interval struct {
	Start int
	End   int
}

type PartInterval map[byte]Interval

func parseInput(input []string) (Workflows, []Part) {
	workflows := Workflows{}
	parts := []Part{}

	i := 0
	for ; input[i] != ""; i++ {
		workflowName, rules := parseWorkflow(input[i])
		workflows[workflowName] = rules
	}

	for i := i + 1; i < len(input); i++ {
		part := parsePart(input[i])
		parts = append(parts, part)
	}

	return workflows, parts
}

func parseWorkflow(line string) (string, []Rule) {
	idx := strings.Index(line, "{")

	workflowName := line[0:idx]
	rules := []Rule{}

	rulesStr := strings.Split(line[idx+1:len(line)-1], ",")
	for _, ruleStr := range rulesStr {
		rule := Rule{}
		idx = strings.Index(ruleStr, ":")
		if idx == -1 {
			rule.WorkflowName = ruleStr
		} else {
			rule.Category = byte(ruleStr[0])
			rule.Operator = byte(ruleStr[1])
			rule.Num, _ = strconv.Atoi(ruleStr[2:idx])
			rule.WorkflowName = ruleStr[idx+1:]
		}

		rules = append(rules, rule)
	}

	return workflowName, rules
}

func parsePart(line string) Part {
	var x, m, a, s int
	_, err := fmt.Sscanf(line, "{x=%d,m=%d,a=%d,s=%d}", &x, &m, &a, &s)
	if err != nil {
		panic(err)
	}

	part := Part{
		'x': x,
		'm': m,
		'a': a,
		's': s,
	}
	return part
}

func applyWorkflow(part Part, workflows Workflows, workflowName string) bool {
	if workflowName == "A" {
		return true
	}
	if workflowName == "R" {
		return false
	}

	for _, rule := range workflows[workflowName] {
		rating := part[rule.Category]
		var isValid = true
		switch rule.Operator {
		case '>':
			isValid = rating > rule.Num
		case '<':
			isValid = rating < rule.Num
		default:
			isValid = true
		}

		if isValid {
			return applyWorkflow(part, workflows, rule.WorkflowName)
		}
	}

	return false
}

func applyWorkflowInterval(partInterval PartInterval, workflows Workflows, workflowName string) int {
	if workflowName == "A" {
		var res = 1
		for _, interval := range partInterval {
			res *= interval.End - interval.Start + 1
		}
		return res
	}
	if workflowName == "R" {
		return 0
	}

	res := 0
	for _, rule := range workflows[workflowName] {
		ratingInterval := partInterval[rule.Category]
		var validRatingInterval Interval
		var invalidRatingInterval Interval

		switch rule.Operator {
		case '>':
			invalidRatingInterval = Interval{ratingInterval.Start, rule.Num}
			validRatingInterval = Interval{rule.Num + 1, ratingInterval.End}
		case '<':
			validRatingInterval = Interval{ratingInterval.Start, rule.Num - 1}
			invalidRatingInterval = Interval{rule.Num, ratingInterval.End}
		default:
			validRatingInterval = ratingInterval
		}

		newPart := PartInterval{}
		for key, value := range partInterval {
			if key == rule.Category {
				newPart[rule.Category] = validRatingInterval
			} else {
				newPart[key] = value
			}
		}
		res += applyWorkflowInterval(newPart, workflows, rule.WorkflowName)

		partInterval[rule.Category] = invalidRatingInterval
	}

	return res
}

func solve(input []string) int {
	startWorflow := "in"
	minRating := 1
	maxRating := 4000

	workflows, _ := parseInput(input)
	partInterval := PartInterval{
		'x': Interval{minRating, maxRating},
		'm': Interval{minRating, maxRating},
		'a': Interval{minRating, maxRating},
		's': Interval{minRating, maxRating},
	}

	res := applyWorkflowInterval(partInterval, workflows, startWorflow)

	return res
}
func readFile(fileName string) []string {
	file, err := os.ReadFile(fileName)
	if err != nil {
		panic(err)
	}

	return strings.Split(strings.TrimSpace(string(file)), "\n")
}

func main() {
	input := readFile("input.txt")
	fmt.Println(solve(input))
}