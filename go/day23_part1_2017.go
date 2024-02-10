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

	var mulCount int
	var pointer int
	registers := make(map[string]int)
	instructions := []string{}

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		instructions = append(instructions, scanner.Text())
	}

	for pointer >= 0 && pointer < len(instructions) {
		parts := strings.Fields(instructions[pointer])
		cmd, x, y := parts[0], parts[1], parts[2]

		getValue := func(s string) int {
			if v, err := strconv.Atoi(s); err == nil {
				return v
			}
			return registers[s]
		}

		switch cmd {
		case "set":
			registers[x] = getValue(y)
		case "sub":
			registers[x] -= getValue(y)
		case "mul":
			registers[x] *= getValue(y)
			mulCount++
		case "jnz":
			if getValue(x) != 0 {
				pointer += getValue(y) - 1
			}
		}
		pointer++
	}

	fmt.Println(mulCount)
}