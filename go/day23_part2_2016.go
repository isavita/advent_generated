package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	// Read instructions from file
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

	// Initialize registers, with register a starting at 12
	registers := map[string]int{"a": 12, "b": 0, "c": 0, "d": 0}

	// Execute instructions
	i := 0
	for i < len(instructions) {
		parts := strings.Fields(instructions[i])
		switch parts[0] {
		case "cpy":
			val, err := strconv.Atoi(parts[1])
			if err != nil {
				val = registers[parts[1]]
			}
			registers[parts[2]] = val
		case "inc":
			registers[parts[1]]++
		case "dec":
			registers[parts[1]]--
		case "jnz":
			val, err := strconv.Atoi(parts[1])
			if err != nil {
				val = registers[parts[1]]
			}
			if val != 0 {
				jump, err := strconv.Atoi(parts[2])
				if err != nil {
					jump = registers[parts[2]]
				}
				i += jump
				continue
			}
		case "tgl":
			targetIndex := i + registers[parts[1]]
			if targetIndex < len(instructions) {
				targetParts := strings.Fields(instructions[targetIndex])
				switch len(targetParts) {
				case 2: // one-argument instruction
					if targetParts[0] == "inc" {
						targetParts[0] = "dec"
					} else {
						targetParts[0] = "inc"
					}
				case 3: // two-argument instruction
					if targetParts[0] == "jnz" {
						targetParts[0] = "cpy"
					} else {
						targetParts[0] = "jnz"
					}
				}
				instructions[targetIndex] = strings.Join(targetParts, " ")
			}
		}
		i++
	}

	fmt.Println(registers["a"])
}