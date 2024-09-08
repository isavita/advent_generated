package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func evaluatePart1(expr string) int64 {
	expr = strings.ReplaceAll(expr, " ", "")
	var result, num int64
	var op byte = '+'

	for i := 0; i < len(expr); i++ {
		if expr[i] >= '0' && expr[i] <= '9' {
			num = num*10 + int64(expr[i]-'0')
		} else if expr[i] == '(' {
			depth := 1
			j := i + 1
			for depth > 0 {
				if expr[j] == '(' {
					depth++
				} else if expr[j] == ')' {
					depth--
				}
				j++
			}
			num = evaluatePart1(expr[i+1 : j-1])
			i = j - 1
		} else {
			if op == '+' {
				result += num
			} else {
				result *= num
			}
			op = expr[i]
			num = 0
		}
	}

	if op == '+' {
		result += num
	} else {
		result *= num
	}

	return result
}

func evaluatePart2(expr string) int64 {
	expr = strings.ReplaceAll(expr, " ", "")
	for strings.Contains(expr, "(") {
		start := strings.LastIndex(expr, "(")
		end := start + strings.Index(expr[start:], ")") + 1
		subExpr := evaluatePart2(expr[start+1 : end-1])
		expr = expr[:start] + strconv.FormatInt(subExpr, 10) + expr[end:]
	}

	terms := strings.Split(expr, "*")
	var result int64 = 1
	for _, term := range terms {
		addends := strings.Split(term, "+")
		var sum int64
		for _, addend := range addends {
			num, _ := strconv.ParseInt(addend, 10, 64)
			sum += num
		}
		result *= sum
	}
	return result
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var sumPart1, sumPart2 int64

	for scanner.Scan() {
		expr := scanner.Text()
		sumPart1 += evaluatePart1(expr)
		sumPart2 += evaluatePart2(expr)
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	fmt.Println("Part 1 result:", sumPart1)
	fmt.Println("Part 2 result:", sumPart2)
}
