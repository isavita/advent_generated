module main

import os
import strconv

struct OpcodeComputer {
mut:
	instructions       []Instruction
	registers          []int
	instruction_pointer int
}

struct Instruction {
	name      string
	abc_values []int
}

fn main() {
	input := os.read_file('input.txt') or { panic(err) }
	println(solve(input))
}

fn solve(input string) int {
	mut opcode_computer := parse_input(input)

	for !opcode_computer.tick() {
		if opcode_computer.registers[opcode_computer.instruction_pointer] == 28 {
			break
		}
	}

	return opcode_computer.registers[5]
}

fn (mut o OpcodeComputer) tick() bool {
	if o.registers[o.instruction_pointer] >= o.instructions.len {
		return true
	}
	inst_index := o.registers[o.instruction_pointer]
	inst := o.instructions[inst_index]
	opcode_func := opcode_names_to_funcs[inst.name]
	o.registers = opcode_func(mut o.registers, inst.abc_values)
	o.registers[o.instruction_pointer]++
	return o.registers[o.instruction_pointer] >= o.instructions.len
}

fn parse_input(input string) OpcodeComputer {
	lines := input.split('\n')
	mut instruction_pointer := 0
	instruction_pointer = strconv.atoi(lines[0][4..]) or { 0 }
	mut instructions := []Instruction{}

	for l in lines[1..] {
		if l.trim_space() == '' {
			continue
		}
		parts := l.split(' ')
		inst := Instruction{
			name: parts[0]
			abc_values: [strconv.atoi(parts[1]) or { 0 }, strconv.atoi(parts[2]) or { 0 }, strconv.atoi(parts[3]) or { 0 }]
		}
		instructions << inst
	}

	return OpcodeComputer{
		instructions: instructions
		instruction_pointer: instruction_pointer
		registers: []int{len: 6}
	}
}

fn addr(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = registers[abc_values[0]] + registers[abc_values[1]]
	return registers
}

fn addi(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = registers[abc_values[0]] + abc_values[1]
	return registers
}

fn mulr(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = registers[abc_values[0]] * registers[abc_values[1]]
	return registers
}

fn muli(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = registers[abc_values[0]] * abc_values[1]
	return registers
}

fn banr(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = registers[abc_values[0]] & registers[abc_values[1]]
	return registers
}

fn bani(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = registers[abc_values[0]] & abc_values[1]
	return registers
}

fn borr(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = registers[abc_values[0]] | registers[abc_values[1]]
	return registers
}

fn bori(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = registers[abc_values[0]] | abc_values[1]
	return registers
}

fn setr(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = registers[abc_values[0]]
	return registers
}

fn seti(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = abc_values[0]
	return registers
}

fn gtir(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = if abc_values[0] > registers[abc_values[1]] { 1 } else { 0 }
	return registers
}

fn gtri(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = if registers[abc_values[0]] > abc_values[1] { 1 } else { 0 }
	return registers
}

fn gtrr(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = if registers[abc_values[0]] > registers[abc_values[1]] { 1 } else { 0 }
	return registers
}

fn eqir(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = if abc_values[0] == registers[abc_values[1]] { 1 } else { 0 }
	return registers
}

fn eqri(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = if registers[abc_values[0]] == abc_values[1] { 1 } else { 0 }
	return registers
}

fn eqrr(mut registers []int, abc_values []int) []int {
	registers[abc_values[2]] = if registers[abc_values[0]] == registers[abc_values[1]] { 1 } else { 0 }
	return registers
}

const opcode_names_to_funcs = {
	'addr': addr
	'addi': addi
	'mulr': mulr
	'muli': muli
	'banr': banr
	'bani': bani
	'borr': borr
	'bori': bori
	'setr': setr
	'seti': seti
	'gtir': gtir
	'gtri': gtri
	'gtrr': gtrr
	'eqir': eqir
	'eqri': eqri
	'eqrr': eqrr
}