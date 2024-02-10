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

func getBoxes(stepsStr []string) map[int][]map[string]int {
	boxes := make(map[int][]map[string]int, hashTableSize)

	for _, stepStr := range stepsStr {
		step := parseStep(stepStr)
		boxContents := boxes[step.NumBox]

		switch step.Operation {
		case "-":
			for i, content := range boxContents {
				if _, ok := content[step.Label]; ok {
					boxContents = append(boxContents[:i], boxContents[i+1:]...)
					break
				}
			}
		case "=":
			found := false
			for _, content := range boxContents {
				if _, ok := content[step.Label]; ok {
					content[step.Label] = step.Number
					found = true
					break
				}
			}
			if !found {
				boxContents = append(boxContents, map[string]int{step.Label: step.Number})
			}
		}

		if len(boxContents) == 0 {
			delete(boxes, step.NumBox)
		} else {
			boxes[step.NumBox] = boxContents
		}
	}

	return boxes
}

func toStringBoxes(boxes map[int][]map[string]int) string {
	res := ""

	for iBox := 0; iBox < hashTableSize; iBox++ {
		if _, ok := boxes[iBox]; ok {
			res += fmt.Sprintf("Box %d : ", iBox)
			for _, content := range boxes[iBox] {
				for key, value := range content {
					res += fmt.Sprintf("[%s %d] ", key, value)
				}
			}
			res += "\n"
		}
	}

	return res
}

func calculatePower(boxes map[int][]map[string]int) int {
	res := 0

	for iBox := 0; iBox < hashTableSize; iBox++ {
		if _, ok := boxes[iBox]; ok {
			for iSlot, content := range boxes[iBox] {
				for _, value := range content {
					res += (iBox + 1) * (iSlot + 1) * value
				}
			}
		}
	}

	return res
}

func solve(input []string) int {
	line := input[0]
	stepsStr := strings.Split(line, ",")

	boxes := getBoxes(stepsStr)

	return calculatePower(boxes)
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