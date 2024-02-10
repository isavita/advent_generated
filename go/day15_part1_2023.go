package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

const hashTableSize = 256

type Step struct {
	Label     string
	NumBox    int
	Operation string
	Number    int
}

func hashString(str string) int {
	res := 0
	for i := 0; i < len(str); i++ {
		char := str[i]
		res += int(char)
		res *= 17
		res %= hashTableSize
	}
	return res
}

func parseStep(stepStr string) Step {
	step := Step{}

	step.Label = strings.TrimRight(stepStr, "=-0123456789")
	step.NumBox = hashString(step.Label)
	step.Operation = stepStr[len(step.Label) : len(step.Label)+1]
	if step.Operation == "=" {
		number, err := strconv.Atoi(stepStr[len(step.Label)+1:])
		if err != nil {
			panic(err)
		}
		step.Number = number
	}

	return step
}

func solve(input []string) int {
	line := input[0]
	steps := strings.Split(line, ",")
	res := 0
	for _, step := range steps {
		res += hashString(step)
	}
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