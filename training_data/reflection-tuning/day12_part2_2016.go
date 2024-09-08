package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Computer struct {
	registers map[string]int
	ip        int
	program   []string
}

func newComputer() *Computer {
	return &Computer{
		registers: map[string]int{"a": 0, "b": 0, "c": 0, "d": 0},
		ip:        0,
	}
}

func (c *Computer) getValue(x string) int {
	if val, err := strconv.Atoi(x); err == nil {
		return val
	}
	return c.registers[x]
}

func (c *Computer) execute(instruction string) {
	parts := strings.Fields(instruction)
	switch parts[0] {
	case "cpy":
		c.registers[parts[2]] = c.getValue(parts[1])
	case "inc":
		c.registers[parts[1]]++
	case "dec":
		c.registers[parts[1]]--
	case "jnz":
		if c.getValue(parts[1]) != 0 {
			c.ip += c.getValue(parts[2]) - 1
		}
	}
	c.ip++
}

func (c *Computer) run() {
	for c.ip < len(c.program) {
		c.execute(c.program[c.ip])
	}
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var program []string
	for scanner.Scan() {
		program = append(program, scanner.Text())
	}

	// Part One
	computer := newComputer()
	computer.program = program
	computer.run()
	fmt.Println("Part One:", computer.registers["a"])

	// Part Two
	computer = newComputer()
	computer.registers["c"] = 1
	computer.program = program
	computer.run()
	fmt.Println("Part Two:", computer.registers["a"])
}
