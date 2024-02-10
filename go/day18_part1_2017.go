package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func getValue(arg string, registers map[string]int) int {
	if val, err := strconv.Atoi(arg); err == nil {
		return val
	}
	return registers[arg]
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var instructions [][]string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		instructions = append(instructions, strings.Fields(scanner.Text()))
	}

	registers := make(map[string]int)
	var lastSound int

	for i := 0; i < len(instructions); {
		instruction := instructions[i]
		cmd := instruction[0]
		arg1 := instruction[1]

		switch cmd {
		case "snd":
			lastSound = getValue(arg1, registers)
		case "set":
			registers[arg1] = getValue(instruction[2], registers)
		case "add":
			registers[arg1] += getValue(instruction[2], registers)
		case "mul":
			registers[arg1] *= getValue(instruction[2], registers)
		case "mod":
			registers[arg1] %= getValue(instruction[2], registers)
		case "rcv":
			if getValue(arg1, registers) != 0 {
				fmt.Println(lastSound)
				return
			}
		case "jgz":
			if getValue(arg1, registers) > 0 {
				i += getValue(instruction[2], registers)
				continue
			}
		}
		i++
	}
}