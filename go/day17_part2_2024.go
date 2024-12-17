package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Program struct {
	a       int64
	b       int64
	c       int64
	program []int
}

func computeOperand(val, a, b, c int64) int64 {
	switch val {
	case 0, 1, 2, 3:
		return val
	case 4:
		return a
	case 5:
		return b
	case 6:
		return c
	default:
		panic(fmt.Sprintf("Invalid combo operand: %d", val))
	}
}

func simulateComputer(program Program) []int {
	outs := make([]int, 0)
	a, b, c := program.a, program.b, program.c
	input := program.program

	for i := 1; i <= len(input); i += 2 {
		cmd := input[i-1]
		switch cmd {
		case 0:
			a >>= computeOperand(int64(input[i]), a, b, c)
		case 1:
			b ^= int64(input[i])
		case 2:
			b = computeOperand(int64(input[i]), a, b, c) % 8
		case 3:
			if a != 0 {
				i = input[i] - 1
			}
		case 4:
			b ^= c
		case 5:
			outs = append(outs, int(computeOperand(int64(input[i]), a, b, c)%8))
		case 6:
			b = a >> computeOperand(int64(input[i]), a, b, c)
		case 7:
			c = a >> computeOperand(int64(input[i]), a, b, c)
		default:
			panic(fmt.Sprintf("Invalid opcode: %d", cmd))
		}
	}
	return outs
}

type Pair struct {
	a int
	b int64
}

func check(p Program) []int64 {
	program := p.program
	valids := make([]int64, 0)
	stack := []Pair{{0, 0}}
	seen := make(map[Pair]bool)

	for len(stack) > 0 {
		state := stack[len(stack)-1]
		stack = stack[:len(stack)-1]

		if seen[state] {
			continue
		}
		seen[state] = true

		depth := state.a
		score := state.b

		if depth == len(program) {
			valids = append(valids, score)
		} else {
			for i := int64(0); i < 8; i++ {
				newScore := i + 8*score
				testProgram := Program{newScore, p.b, p.c, program}
				result := simulateComputer(testProgram)
				if len(result) > 0 && result[0] == program[len(program)-1-depth] {
					stack = append(stack, Pair{depth + 1, newScore})
				}
			}
		}
	}
	return valids
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var a, b, c int64
	var program []int

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if strings.HasPrefix(line, "Register A:") {
			parts := strings.Split(line, ":")
			a, _ = strconv.ParseInt(strings.TrimSpace(parts[1]), 10, 64)
		} else if strings.HasPrefix(line, "Register B:") {
			parts := strings.Split(line, ":")
			b, _ = strconv.ParseInt(strings.TrimSpace(parts[1]), 10, 64)
		} else if strings.HasPrefix(line, "Register C:") {
			parts := strings.Split(line, ":")
			c, _ = strconv.ParseInt(strings.TrimSpace(parts[1]), 10, 64)
		} else if strings.HasPrefix(line, "Program:") {
			parts := strings.Split(line, ":")
			nums := strings.Split(strings.TrimSpace(parts[1]), ",")
			for _, n := range nums {
				val, _ := strconv.Atoi(strings.TrimSpace(n))
				program = append(program, val)
			}
		}
	}

	// Create the program
	p := Program{a, b, c, program}

	// Part 2: Find the minimum valid value
	validValues := check(p)
	minVal := validValues[0]
	for _, val := range validValues {
		if val < minVal {
			minVal = val
		}
	}

	fmt.Println(minVal)
}
