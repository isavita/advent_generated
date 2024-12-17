package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	data, err := ioutil.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	lines := bytes.Split(data, []byte("\n"))
	var A, B, C int
	var program []int

	for _, line := range lines {
		s := string(line)
		s = strings.TrimSpace(s)
		if s == "" {
			continue
		}
		if strings.HasPrefix(s, "Register A:") {
			parts := strings.Split(s, ":")
			A, _ = strconv.Atoi(strings.TrimSpace(parts[1]))
		} else if strings.HasPrefix(s, "Register B:") {
			parts := strings.Split(s, ":")
			B, _ = strconv.Atoi(strings.TrimSpace(parts[1]))
		} else if strings.HasPrefix(s, "Register C:") {
			parts := strings.Split(s, ":")
			C, _ = strconv.Atoi(strings.TrimSpace(parts[1]))
		} else if strings.HasPrefix(s, "Program:") {
			parts := strings.Split(s, ":")
			pStr := strings.TrimSpace(parts[1])
			nums := strings.Split(pStr, ",")
			for _, n := range nums {
				val, _ := strconv.Atoi(strings.TrimSpace(n))
				program = append(program, val)
			}
		}
	}

	// helper to get combo operand value
	getComboVal := func(op int) int {
		switch {
		case op <= 3:
			return op
		case op == 4:
			return A
		case op == 5:
			return B
		case op == 6:
			return C
		default:
			panic("invalid combo operand")
		}
	}

	var outputVals []string
	ip := 0
	for {
		if ip >= len(program) {
			break
		}
		opcode := program[ip]
		if ip+1 >= len(program) {
			break
		}
		operand := program[ip+1]

		switch opcode {
		case 0: // adv
			den := getComboVal(operand)
			if den == 0 {
				A = 0
			} else {
				pow := 1
				for i := 0; i < den; i++ {
					pow *= 2
				}
				A = A / pow
			}
			ip += 2
		case 1: // bxl
			B = B ^ operand
			ip += 2
		case 2: // bst
			val := getComboVal(operand) % 8
			B = val
			ip += 2
		case 3: // jnz
			if A != 0 {
				ip = operand
			} else {
				ip += 2
			}
		case 4: // bxc
			B = B ^ C
			ip += 2
		case 5: // out
			val := getComboVal(operand) % 8
			outputVals = append(outputVals, strconv.Itoa(val))
			ip += 2
		case 6: // bdv
			den := getComboVal(operand)
			pow := 1
			for i := 0; i < den; i++ {
				pow *= 2
			}
			B = A / pow
			ip += 2
		case 7: // cdv
			den := getComboVal(operand)
			pow := 1
			for i := 0; i < den; i++ {
				pow *= 2
			}
			C = A / pow
			ip += 2
		default:
			break
		}
	}

	fmt.Println(strings.Join(outputVals, ","))
}
