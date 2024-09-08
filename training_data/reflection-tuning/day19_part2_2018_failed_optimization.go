package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Instruction struct {
	opcode string
	a, b, c int
}

type CPU struct {
	registers [6]int
	ip        int
	ipBound   int
}

func (cpu *CPU) execute(inst Instruction) {
	switch inst.opcode {
	case "addr":
		cpu.registers[inst.c] = cpu.registers[inst.a] + cpu.registers[inst.b]
	case "addi":
		cpu.registers[inst.c] = cpu.registers[inst.a] + inst.b
	case "mulr":
		cpu.registers[inst.c] = cpu.registers[inst.a] * cpu.registers[inst.b]
	case "muli":
		cpu.registers[inst.c] = cpu.registers[inst.a] * inst.b
	case "banr":
		cpu.registers[inst.c] = cpu.registers[inst.a] & cpu.registers[inst.b]
	case "bani":
		cpu.registers[inst.c] = cpu.registers[inst.a] & inst.b
	case "borr":
		cpu.registers[inst.c] = cpu.registers[inst.a] | cpu.registers[inst.b]
	case "bori":
		cpu.registers[inst.c] = cpu.registers[inst.a] | inst.b
	case "setr":
		cpu.registers[inst.c] = cpu.registers[inst.a]
	case "seti":
		cpu.registers[inst.c] = inst.a
	case "gtir":
		if inst.a > cpu.registers[inst.b] {
			cpu.registers[inst.c] = 1
		} else {
			cpu.registers[inst.c] = 0
		}
	case "gtri":
		if cpu.registers[inst.a] > inst.b {
			cpu.registers[inst.c] = 1
		} else {
			cpu.registers[inst.c] = 0
		}
	case "gtrr":
		if cpu.registers[inst.a] > cpu.registers[inst.b] {
			cpu.registers[inst.c] = 1
		} else {
			cpu.registers[inst.c] = 0
		}
	case "eqir":
		if inst.a == cpu.registers[inst.b] {
			cpu.registers[inst.c] = 1
		} else {
			cpu.registers[inst.c] = 0
		}
	case "eqri":
		if cpu.registers[inst.a] == inst.b {
			cpu.registers[inst.c] = 1
		} else {
			cpu.registers[inst.c] = 0
		}
	case "eqrr":
		if cpu.registers[inst.a] == cpu.registers[inst.b] {
			cpu.registers[inst.c] = 1
		} else {
			cpu.registers[inst.c] = 0
		}
	}
}

func parseInput(filename string) (int, []Instruction) {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)

	scanner.Scan()
	ipBound, _ := strconv.Atoi(strings.Fields(scanner.Text())[1])

	var instructions []Instruction
	for scanner.Scan() {
		fields := strings.Fields(scanner.Text())
		a, _ := strconv.Atoi(fields[1])
		b, _ := strconv.Atoi(fields[2])
		c, _ := strconv.Atoi(fields[3])
		instructions = append(instructions, Instruction{fields[0], a, b, c})
	}

	return ipBound, instructions
}

func runProgram(cpu *CPU, instructions []Instruction) {
	for cpu.ip >= 0 && cpu.ip < len(instructions) {
		cpu.registers[cpu.ipBound] = cpu.ip
		cpu.execute(instructions[cpu.ip])
		cpu.ip = cpu.registers[cpu.ipBound]
		cpu.ip++
	}
}

func main() {
	ipBound, instructions := parseInput("input.txt")

	// Part One
	cpu := &CPU{ipBound: ipBound}
	runProgram(cpu, instructions)
	fmt.Println("Part One:", cpu.registers[0])

	// Part Two
	cpu = &CPU{ipBound: ipBound}
	cpu.registers[0] = 1
	runProgram(cpu, instructions)
	fmt.Println("Part Two:", cpu.registers[0])
}
