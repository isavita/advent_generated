package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	instructions := readInstructions("input.txt")
	registers := map[string]int{"a": 7, "b": 0, "c": 0, "d": 0} // Initialize register 'a' to 7 (number of eggs)
	executeInstructions(instructions, registers)
	fmt.Println(registers["a"])
}

func readInstructions(filename string) []string {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var instructions []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		instructions = append(instructions, scanner.Text())
	}
	return instructions
}

func executeInstructions(instructions []string, registers map[string]int) {
	var pc int // Program counter
	for pc < len(instructions) {
		fields := strings.Fields(instructions[pc])
		switch fields[0] {
		case "cpy":
			x := getValue(fields[1], registers)
			if _, exists := registers[fields[2]]; exists {
				registers[fields[2]] = x
			}
		case "inc":
			if _, exists := registers[fields[1]]; exists {
				registers[fields[1]]++
			}
		case "dec":
			if _, exists := registers[fields[1]]; exists {
				registers[fields[1]]--
			}
		case "jnz":
			x := getValue(fields[1], registers)
			if x != 0 {
				pc += getValue(fields[2], registers) - 1
			}
		case "tgl":
			x := getValue(fields[1], registers)
			if tgt := pc + x; tgt >= 0 && tgt < len(instructions) {
				instructions[tgt] = toggleInstruction(instructions[tgt])
			}
		}
		pc++
	}
}

func getValue(s string, registers map[string]int) int {
	if val, err := strconv.Atoi(s); err == nil {
		return val
	}
	return registers[s]
}

func toggleInstruction(instr string) string {
	parts := strings.Fields(instr)
	switch parts[0] {
	case "inc":
		parts[0] = "dec"
	case "dec", "tgl":
		parts[0] = "inc"
	case "jnz":
		parts[0] = "cpy"
	case "cpy":
		parts[0] = "jnz"
	}
	return strings.Join(parts, " ")
}