package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Instruction struct {
	op  string
	x   string
	y   string
}

func main() {
	instructions := parseInput("input.txt")
	result := execute(instructions)
	fmt.Println(result)
}

func parseInput(filename string) []Instruction {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var instructions []Instruction
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		inst := Instruction{op: parts[0], x: parts[1]}
		if len(parts) > 2 {
			inst.y = parts[2]
		}
		instructions = append(instructions, inst)
	}
	return instructions
}

func execute(instructions []Instruction) int {
	registers := make(map[string]int)
	lastSound := 0
	pc := 0

	getValue := func(s string) int {
		if val, err := strconv.Atoi(s); err == nil {
			return val
		}
		return registers[s]
	}

	for pc >= 0 && pc < len(instructions) {
		inst := instructions[pc]
		switch inst.op {
		case "snd":
			lastSound = getValue(inst.x)
		case "set":
			registers[inst.x] = getValue(inst.y)
		case "add":
			registers[inst.x] += getValue(inst.y)
		case "mul":
			registers[inst.x] *= getValue(inst.y)
		case "mod":
			registers[inst.x] %= getValue(inst.y)
		case "rcv":
			if getValue(inst.x) != 0 {
				return lastSound
			}
		case "jgz":
			if getValue(inst.x) > 0 {
				pc += getValue(inst.y) - 1
			}
		}
		pc++
	}
	return lastSound
}
