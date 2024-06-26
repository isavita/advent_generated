package main

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

type Instruction struct {
	left  string
	right string
}

const ElemToMatch = "ZZZ"

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(file))

	re := regexp.MustCompile("[A-Z]{3}")

	lines := strings.Split(input, "\n")

	desertMap := make(map[string]Instruction)

	// Ignore the first two lines since they are the instruction set and a blank line
	for _, line := range lines[2:] {
		if len(line) == 0 {
			continue
		}

		matches := re.FindAllString(line, -1)
		desertMap[matches[0]] = Instruction{
			left:  matches[1],
			right: matches[2],
		}
	}

	current := "AAA"
	steps := 0

	for current != ElemToMatch {
		for i := range strings.TrimSpace(lines[0]) {
			direction := lines[0][i]
			if direction == 'R' {
				current = desertMap[current].right
			} else if direction == 'L' {
				current = desertMap[current].left
			}
			steps++

			if current == ElemToMatch {
				break
			}
		}
	}

	fmt.Println(steps)
}