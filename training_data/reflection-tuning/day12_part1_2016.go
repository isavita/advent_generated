package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Instruction struct {
	op   string
	args []string
}

type Computer struct {
	registers map[string]int
	ip        int
	program   []Instruction
}

func newComputer(program []Instruction) *Computer {
	return &Computer{
		registers: map[string]int{"a": 0, "b": 0, "c": 0, "d": 0},
		ip:        0,
		program:   program,
	}
}

func (c *Computer) getValue(x string) int {
	if val, err := strconv.Atoi(x); err == nil {
		return val
	}
	return c.registers[x]
}

func (c *Computer) execute() {
	for c.ip < len(c.program) {
		inst := c.program[c.ip]
		switch inst.op {
		case "cpy":
			c.registers[inst.args[1]] = c.getValue(inst.args[0])
		case "inc":
			c.registers[inst.args[0]]++
		case "dec":
			c.registers[inst.args[0]]--
		case "jnz":
			if c.getValue(inst.args[0]) != 0 {
				c.ip += c.getValue(inst.args[1]) - 1
			}
		}
		c.ip++
	}
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	var program []Instruction
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		program = append(program, Instruction{op: parts[0], args: parts[1:]})
	}

	computer := newComputer(program)
	computer.execute()
	fmt.Println(computer.registers["a"])
}
