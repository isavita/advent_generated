package main

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading from file:", err)
	}
	input := strings.TrimSpace(string(file))
	ans := solve(input)
	fmt.Println(ans)
}

func solve(input string) int {
	lines := parseInput(input)
	var total int

	for _, line := range lines {
		total += doMaths(line, calcFlatSlicePart)
	}

	return total
}

func parseInput(input string) (ans [][]string) {
	lines := strings.Split(input, "\n")
	for _, l := range lines {

		ans = append(ans, strings.Split(strings.ReplaceAll(l, " ", ""), ""))
	}
	return ans
}

func doMaths(input []string, flatteningFunc func([]string) string) int {
	var stackOpenIndices []int
	var stackFlattened []string
	for i := 0; i < len(input); i++ {

		stackFlattened = append(stackFlattened, input[i])
		switch input[i] {
		case "(":
			stackOpenIndices = append(stackOpenIndices, len(stackFlattened)-1)
		case ")":

			openIndex := stackOpenIndices[len(stackOpenIndices)-1]
			stackOpenIndices = stackOpenIndices[:len(stackOpenIndices)-1]

			sliToFlatten := stackFlattened[openIndex+1 : len(stackFlattened)-1]
			stackFlattened[openIndex] = flatteningFunc(sliToFlatten)

			stackFlattened = stackFlattened[:openIndex+1]
		}

	}

	return toInt(flatteningFunc(stackFlattened))
}

func calcFlatSlicePart1(input []string) string {
	for _, v := range input {
		if v == "(" || v == ")" {
			panic(fmt.Sprintf("unexpected paren in flat input, %v", input))
		}
	}

	result := toInt(input[0])

	for i := range input {
		if i+2 < len(input) {
			switch input[i+1] {
			case "+":
				result += toInt(input[i+2])
			case "*":
				result *= toInt(input[i+2])
			}
		}
	}

	return toString(result)
}

func calcFlatSlicePart(input []string) string {
	for _, v := range input {
		if v == "(" || v == ")" {
			panic(fmt.Sprintf("unexpected paren in flat input, %v", input))
		}
	}

	for i := 1; i < len(input)-1; i++ {
		if input[i] == "+" {
			toLeft := input[i-1]
			toRight := input[i+1]
			if isNum(toLeft) && isNum(toRight) {
				input[i-1] = addStrings(toLeft, toRight)
				input = splice(input, i, 2)
				i--
			}
		}
	}

	for i := 1; i < len(input)-1; i++ {
		if input[i] == "*" {
			toLeft := input[i-1]
			toRight := input[i+1]
			if isNum(toLeft) && isNum(toRight) {
				input[i-1] = multiplyStrings(toLeft, toRight)
				input = splice(input, i, 2)
				i--
			}
		}
	}

	return input[0]
}

var numReg = regexp.MustCompile("[0-9]")

func isNum(str string) bool {
	return numReg.MatchString(str)
}

func addStrings(strs ...string) string {
	var sum int
	for _, str := range strs {
		sum += toInt(str)
	}
	return toString(sum)
}
func multiplyStrings(strs ...string) string {
	sum := 1
	for _, str := range strs {
		sum *= toInt(str)
	}
	return toString(sum)
}

func splice(sli []string, startIndex, items int) []string {
	copy(sli[startIndex:], sli[startIndex+items:])
	sli = sli[:len(sli)-items]
	return sli
}

func toInt(s string) int {
	n, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return n
}

func toString(n int) string {
	return strconv.Itoa(n)
}