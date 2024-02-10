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
	var instructions []string

	for scanner.Scan() {
		instructions = append(instructions, scanner.Text())
	}

	for i := 0; i < len(instructions); i++ {
		op, arg := parseInstruction(instructions[i])
		if op == "acc" {
			continue
		}

		modifiedInstructions := make([]string, len(instructions))
		copy(modifiedInstructions, instructions)
		if op == "jmp" {
			modifiedInstructions[i] = fmt.Sprintf("nop %d", arg)
		} else {
			modifiedInstructions[i] = fmt.Sprintf("jmp %d", arg)
		}

		if accumulator, terminated := executeBootCode(modifiedInstructions); terminated {
			fmt.Println(accumulator)
			break
		}
	}
}

func executeBootCode(instructions []string) (int, bool) {
	accumulator := 0
	visited := make(map[int]bool)
	currentInstruction := 0

	for currentInstruction < len(instructions) {
		if visited[currentInstruction] {
			return accumulator, false
		}

		visited[currentInstruction] = true
		op, arg := parseInstruction(instructions[currentInstruction])

		switch op {
		case "acc":
			accumulator += arg
			currentInstruction++
		case "jmp":
			currentInstruction += arg
		case "nop":
			currentInstruction++
		}
	}

	return accumulator, true
}

func parseInstruction(instruction string) (string, int) {
	parts := strings.Fields(instruction)
	op := parts[0]
	arg, _ := strconv.Atoi(parts[1])
	return op, arg
}