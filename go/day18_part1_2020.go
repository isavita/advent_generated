package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	sum := 0
	for scanner.Scan() {
		expression := scanner.Text()
		result := evaluate(expression)
		sum += result
	}

	fmt.Println(sum)
}

func evaluate(expression string) int {
	tokens := tokenize(expression)
	return evaluateTokens(tokens)
}

func tokenize(expression string) []string {
	expression = strings.ReplaceAll(expression, "(", "( ")
	expression = strings.ReplaceAll(expression, ")", " )")
	return strings.Fields(expression)
}

func evaluateTokens(tokens []string) int {
	ops := []string{}
	vals := []int{}

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		switch token {
		case "(":
			ops = append(ops, token)
		case "+", "*":
			for len(ops) > 0 && ops[len(ops)-1] != "(" {
				vals = append(vals[:len(vals)-2], applyOp(ops[len(ops)-1], vals[len(vals)-2], vals[len(vals)-1]))
				ops = ops[:len(ops)-1]
			}
			ops = append(ops, token)
		case ")":
			for ops[len(ops)-1] != "(" {
				vals = append(vals[:len(vals)-2], applyOp(ops[len(ops)-1], vals[len(vals)-2], vals[len(vals)-1]))
				ops = ops[:len(ops)-1]
			}
			ops = ops[:len(ops)-1] // Remove the opening '('
		default:
			value, _ := strconv.Atoi(token)
			vals = append(vals, value)
		}
	}
	for len(ops) > 0 {
		vals = append(vals[:len(vals)-2], applyOp(ops[len(ops)-1], vals[len(vals)-2], vals[len(vals)-1]))
		ops = ops[:len(ops)-1]
	}
	return vals[0]
}

func applyOp(op string, a, b int) int {
	switch op {
	case "+":
		return a + b
	case "*":
		return a * b
	default:
		panic(fmt.Sprintf("Unknown operator: %s", op))
	}
}