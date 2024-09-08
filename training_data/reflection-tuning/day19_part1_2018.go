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

type Machine struct {
	registers    [6]int
	ip           int
	ipBinding    int
	instructions []Instruction
}

func (m *Machine) execute(inst Instruction) {
	switch inst.opcode {
	case "seti":
		m.registers[inst.c] = inst.a
	case "setr":
		m.registers[inst.c] = m.registers[inst.a]
	case "addi":
		m.registers[inst.c] = m.registers[inst.a] + inst.b
	case "addr":
		m.registers[inst.c] = m.registers[inst.a] + m.registers[inst.b]
	case "muli":
		m.registers[inst.c] = m.registers[inst.a] * inst.b
	case "mulr":
		m.registers[inst.c] = m.registers[inst.a] * m.registers[inst.b]
	case "bani":
		m.registers[inst.c] = m.registers[inst.a] & inst.b
	case "banr":
		m.registers[inst.c] = m.registers[inst.a] & m.registers[inst.b]
	case "bori":
		m.registers[inst.c] = m.registers[inst.a] | inst.b
	case "borr":
		m.registers[inst.c] = m.registers[inst.a] | m.registers[inst.b]
	case "gtir":
		if inst.a > m.registers[inst.b] {
			m.registers[inst.c] = 1
		} else {
			m.registers[inst.c] = 0
		}
	case "gtri":
		if m.registers[inst.a] > inst.b {
			m.registers[inst.c] = 1
		} else {
			m.registers[inst.c] = 0
		}
	case "gtrr":
		if m.registers[inst.a] > m.registers[inst.b] {
			m.registers[inst.c] = 1
		} else {
			m.registers[inst.c] = 0
		}
	case "eqir":
		if inst.a == m.registers[inst.b] {
			m.registers[inst.c] = 1
		} else {
			m.registers[inst.c] = 0
		}
	case "eqri":
		if m.registers[inst.a] == inst.b {
			m.registers[inst.c] = 1
		} else {
			m.registers[inst.c] = 0
		}
	case "eqrr":
		if m.registers[inst.a] == m.registers[inst.b] {
			m.registers[inst.c] = 1
		} else {
			m.registers[inst.c] = 0
		}
	}
}

func (m *Machine) run() {
	for m.ip >= 0 && m.ip < len(m.instructions) {
		m.registers[m.ipBinding] = m.ip
		m.execute(m.instructions[m.ip])
		m.ip = m.registers[m.ipBinding]
		m.ip++
	}
}

func parseInput(filename string) *Machine {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	machine := &Machine{}

	// Parse IP binding
	scanner.Scan()
	fmt.Sscanf(scanner.Text(), "#ip %d", &machine.ipBinding)

	// Parse instructions
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		a, _ := strconv.Atoi(parts[1])
		b, _ := strconv.Atoi(parts[2])
		c, _ := strconv.Atoi(parts[3])
		machine.instructions = append(machine.instructions, Instruction{parts[0], a, b, c})
	}

	return machine
}

func main() {
	machine := parseInput("input.txt")
	machine.run()
	fmt.Println(machine.registers[0])
}
