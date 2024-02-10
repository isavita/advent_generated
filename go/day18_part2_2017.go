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

	registers0 := map[string]int{"p": 0}
	registers1 := map[string]int{"p": 1}
	queue0 := []int{}
	queue1 := []int{}
	sendCount1 := 0
	i0, i1 := 0, 0
	deadlock0, deadlock1 := false, false

	for !(deadlock0 && deadlock1) {
		deadlock0, deadlock1 = true, true

		// Program 0
		for i0 < len(instructions) {
			instruction := instructions[i0]
			cmd := instruction[0]
			arg1 := instruction[1]
			switch cmd {
			case "snd":
				queue1 = append(queue1, getValue(arg1, registers0))
			case "set":
				registers0[arg1] = getValue(instruction[2], registers0)
			case "add":
				registers0[arg1] += getValue(instruction[2], registers0)
			case "mul":
				registers0[arg1] *= getValue(instruction[2], registers0)
			case "mod":
				registers0[arg1] %= getValue(instruction[2], registers0)
			case "rcv":
				if len(queue0) == 0 {
					goto EndProgram0
				}
				registers0[arg1] = queue0[0]
				queue0 = queue0[1:]
			case "jgz":
				if getValue(arg1, registers0) > 0 {
					i0 += getValue(instruction[2], registers0)
					continue
				}
			}
			i0++
			deadlock0 = false
		}

	EndProgram0:

		// Program 1
		for i1 < len(instructions) {
			instruction := instructions[i1]
			cmd := instruction[0]
			arg1 := instruction[1]
			switch cmd {
			case "snd":
				queue0 = append(queue0, getValue(arg1, registers1))
				sendCount1++
			case "set":
				registers1[arg1] = getValue(instruction[2], registers1)
			case "add":
				registers1[arg1] += getValue(instruction[2], registers1)
			case "mul":
				registers1[arg1] *= getValue(instruction[2], registers1)
			case "mod":
				registers1[arg1] %= getValue(instruction[2], registers1)
			case "rcv":
				if len(queue1) == 0 {
					goto EndProgram1
				}
				registers1[arg1] = queue1[0]
				queue1 = queue1[1:]
			case "jgz":
				if getValue(arg1, registers1) > 0 {
					i1 += getValue(instruction[2], registers1)
					continue
				}
			}
			i1++
			deadlock1 = false
		}

	EndProgram1:
	}

	fmt.Println(sendCount1)
}