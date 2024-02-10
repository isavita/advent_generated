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

	var instructions []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		instructions = append(instructions, scanner.Text())
	}

	for a := 1; ; a++ {
		if producesClockSignal(a, instructions) {
			fmt.Println(a)
			break
		}
	}
}

func producesClockSignal(a int, instructions []string) bool {
	registers := map[string]int{"a": a, "b": 0, "c": 0, "d": 0}
	var lastOutput, outputCount int

	for i := 0; i < len(instructions); {
		parts := strings.Fields(instructions[i])
		switch parts[0] {
		case "cpy":
			val := getValue(parts[1], registers)
			registers[parts[2]] = val
		case "inc":
			registers[parts[1]]++
		case "dec":
			registers[parts[1]]--
		case "jnz":
			val := getValue(parts[1], registers)
			if val != 0 {
				jump, _ := strconv.Atoi(parts[2])
				i += jump
				continue
			}
		case "out":
			val := getValue(parts[1], registers)
			if val != 0 && val != 1 {
				return false
			}
			if outputCount > 0 && val == lastOutput {
				return false
			}
			lastOutput = val
			outputCount++
			if outputCount > 50 {
				return true
			}
		}
		i++
	}
	return false
}

func getValue(s string, registers map[string]int) int {
	val, err := strconv.Atoi(s)
	if err != nil {
		return registers[s]
	}
	return val
}