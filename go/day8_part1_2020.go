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

	accumulator, _ := executeBootCode(instructions)
	fmt.Println(accumulator)
}

func executeBootCode(instructions []string) (int, bool) {
	accumulator := 0
	visited := make(map[int]bool)
	currentInstruction := 0

	for currentInstruction < len(instructions) {
		if visited[currentInstruction] {
			return accumulator, true
		}

		visited[currentInstruction] = true
		parts := strings.Fields(instructions[currentInstruction])
		op := parts[0]
		arg, _ := strconv.Atoi(parts[1])

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

	return accumulator, false
}