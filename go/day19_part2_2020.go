package main

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	file, _ := os.ReadFile("input.txt")
	input := strings.TrimSpace(string(file))
	result := solve(input)
	fmt.Println(result)
}

func solve(input string) int {
	graph, messages := parseInput(input)

	fillInGraph(graph, 42)
	fillInGraph(graph, 31)

	part42 := fmt.Sprintf("(%s)", strings.Join(graph[42].resolved, "|"))
	part31 := fmt.Sprintf("(%s)", strings.Join(graph[31].resolved, "|"))

	rule8String := fmt.Sprintf("(%s)+", part42)

	makeRegexp := func(num int) *regexp.Regexp {

		return regexp.MustCompile(fmt.Sprintf("^%s%s{%d}%s{%d}$", rule8String, part42, num, part31, num))
	}

	var matchRuleZero int
	for _, m := range messages {
		for i := 1; i < 10; i++ {
			pattern := makeRegexp(i)
			if pattern.MatchString(m) {
				matchRuleZero++
				break
			}
		}
	}

	return matchRuleZero
}

func fillInGraph(graph map[int]*rule, entry int) []string {
	if len(graph[entry].resolved) != 0 {
		return append([]string{}, graph[entry].resolved...)
	}

	for _, option := range graph[entry].options {
		resolved := []string{""}
		for _, entryPoint := range option {
			nestedResolveVals := fillInGraph(graph, entryPoint)
			var newResolved []string
			for _, nextPiece := range nestedResolveVals {
				for _, prev := range resolved {
					newResolved = append(newResolved, prev+nextPiece)
				}
			}
			resolved = newResolved
		}
		graph[entry].resolved = append(graph[entry].resolved, resolved...)
	}

	return graph[entry].resolved
}

type rule struct {
	resolved []string
	options  [][]int
}

func (r rule) String() string {
	var ans string
	ans += fmt.Sprintf("OPTIONS:  %v\n", r.options)
	ans += fmt.Sprintf(" RESOLVED: %v\n", r.resolved)
	return ans
}

func parseInput(input string) (rules map[int]*rule, messages []string) {
	parts := strings.Split(input, "\n\n")

	rules = map[int]*rule{}
	for _, r := range strings.Split(parts[0], "\n") {
		if regexp.MustCompile("[a-z]").MatchString(r) {
			var num int
			var char string
			fmt.Sscanf(r, "%d: \"%1s\"", &num, &char)
			rules[num] = &rule{resolved: []string{char}}
		} else {
			split := strings.Split(r, ": ")
			key := toInt(split[0])
			newRule := rule{}
			for _, ruleNums := range strings.Split(split[1], " | ") {
				nums := strings.Split(ruleNums, " ")
				var option []int
				for _, n := range nums {
					option = append(option, toInt(n))
				}
				newRule.options = append(newRule.options, option)
			}
			rules[key] = &newRule
		}
	}

	messages = strings.Split(parts[1], "\n")

	return rules, messages
}

func toInt(s string) int {
	n, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return n
}