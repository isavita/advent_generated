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
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	sum := 0
	for scanner.Scan() {
		sum += evaluate(scanner.Text())
	}

	fmt.Println(sum)
}

func evaluate(expr string) int {
	expr = strings.ReplaceAll(expr, " ", "")
	for strings.Contains(expr, "(") {
		expr = evaluateParentheses(expr)
	}
	return evaluateSimple(expr)
}

func evaluateParentheses(expr string) string {
	start, end := -1, -1
	for i, ch := range expr {
		if ch == '(' {
			start = i
		} else if ch == ')' {
			end = i
			break
		}
	}
	if start != -1 && end != -1 {
		result := evaluateSimple(expr[start+1 : end])
		return expr[:start] + strconv.Itoa(result) + expr[end+1:]
	}
	return expr
}

func evaluateSimple(expr string) int {
	nums := []int{}
	ops := []byte{}
	num := 0
	for i := 0; i < len(expr); i++ {
		if expr[i] >= '0' && expr[i] <= '9' {
			num = num*10 + int(expr[i]-'0')
		} else {
			nums = append(nums, num)
			num = 0
			ops = append(ops, expr[i])
		}
	}
	nums = append(nums, num)

	result := nums[0]
	for i, op := range ops {
		if op == '+' {
			result += nums[i+1]
		} else {
			result *= nums[i+1]
		}
	}
	return result
}
