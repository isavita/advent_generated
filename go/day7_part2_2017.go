package main

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type Program struct {
	Weight int
	Holds  []string
}

func dfs(name string, programs map[string]Program) (int, bool) {
	program := programs[name]
	totalWeight := program.Weight

	weights := make(map[int]int)
	for _, child := range program.Holds {
		weight, balanced := dfs(child, programs)
		if !balanced {
			return 0, false
		}
		totalWeight += weight
		weights[weight]++
	}

	// Check for unbalance
	for w1, c1 := range weights {
		for w2, c2 := range weights {
			if w1 != w2 && c1 < c2 {
				unbalancedProgram := ""
				for _, child := range program.Holds {
					if childWeight, _ := dfs(child, programs); childWeight == w1 {
						unbalancedProgram = child
						break
					}
				}
				fmt.Println(programs[unbalancedProgram].Weight + (w2 - w1))
				return 0, false
			}
		}
	}
	return totalWeight, true
}

func main() {
	// Step 1: Read Input
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}

	lines := strings.Split(strings.TrimSpace(string(data)), "\n")

	// Step 2: Create Data Structure
	programs := make(map[string]Program)

	re := regexp.MustCompile(`[a-z]+|\d+`)

	for _, line := range lines {
		matches := re.FindAllString(line, -1)
		name := matches[0]
		weight, _ := strconv.Atoi(matches[1])

		program := Program{Weight: weight}
		if len(matches) > 2 {
			program.Holds = matches[2:]
		}
		programs[name] = program
	}

	// Step 4: Find Root (from Part One)
	root := "dtacyn" // Replace this with the root found in Part One

	// Step 5: Identify Unbalance
	dfs(root, programs)
}