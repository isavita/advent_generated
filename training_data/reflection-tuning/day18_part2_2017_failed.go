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
	reg string
	val string
}

type Program struct {
	id        int
	registers map[string]int
	pc        int
	sendCount int
}

func parseInstructions(input []string) []Instruction {
	instructions := make([]Instruction, len(input))
	for i, line := range input {
		parts := strings.Fields(line)
		if len(parts) == 2 {
			instructions[i] = Instruction{op: parts[0], reg: parts[1]}
		} else {
			instructions[i] = Instruction{op: parts[0], reg: parts[1], val: parts[2]}
		}
	}
	return instructions
}

func getValue(p *Program, s string) int {
	if v, err := strconv.Atoi(s); err == nil {
		return v
	}
	return p.registers[s]
}

func runProgram(id int, instructions []Instruction, send chan<- int, receive <-chan int, done chan<- bool) {
	p := &Program{id: id, registers: make(map[string]int), pc: 0, sendCount: 0}
	p.registers["p"] = id

	for p.pc >= 0 && p.pc < len(instructions) {
		inst := instructions[p.pc]
		switch inst.op {
		case "snd":
			send <- getValue(p, inst.reg)
			p.sendCount++
		case "set":
			p.registers[inst.reg] = getValue(p, inst.val)
		case "add":
			p.registers[inst.reg] += getValue(p, inst.val)
		case "mul":
			p.registers[inst.reg] *= getValue(p, inst.val)
		case "mod":
			p.registers[inst.reg] %= getValue(p, inst.val)
		case "rcv":
			p.registers[inst.reg] = <-receive
		case "jgz":
			if getValue(p, inst.reg) > 0 {
				p.pc += getValue(p, inst.val) - 1
			}
		}
		p.pc++
	}
	done <- true
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	var input []string
	for scanner.Scan() {
		input = append(input, scanner.Text())
	}

	instructions := parseInstructions(input)

	ch0to1 := make(chan int, 1000)
	ch1to0 := make(chan int, 1000)
	done0 := make(chan bool)
	done1 := make(chan bool)

	go runProgram(0, instructions, ch0to1, ch1to0, done0)
	go runProgram(1, instructions, ch1to0, ch0to1, done1)

	<-done0
	<-done1

	close(ch0to1)
	close(ch1to0)

	fmt.Println("Program 1 send count:", len(ch1to0))
}
