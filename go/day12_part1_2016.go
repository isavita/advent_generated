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
	instructions := []string{}
	for scanner.Scan() {
		instructions = append(instructions, scanner.Text())
	}

	registers := map[string]int{"a": 0, "b": 0, "c": 0, "d": 0}
	executeInstructions(instructions, registers)

	fmt.Println(registers["a"])
}

func executeInstructions(instructions []string, registers map[string]int) {
	for i := 0; i < len(instructions); {
		parts := strings.Fields(instructions[i])
		switch parts[0] {
		case "cpy":
			val := getValue(parts[1], registers)
			registers[parts[2]] = val
			i++
		case "inc":
			registers[parts[1]]++
			i++
		case "dec":
			registers[parts[1]]--
			i++
		case "jnz":
			val := getValue(parts[1], registers)
			if val != 0 {
				jump, _ := strconv.Atoi(parts[2])
				i += jump
			} else {
				i++
			}
		}
	}
}

func getValue(s string, registers map[string]int) int {
	val, err := strconv.Atoi(s)
	if err != nil {
		return registers[s]
	}
	return val
}