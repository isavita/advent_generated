package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	// Step 1: Read Input
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}
	defer file.Close()

	// Step 2: Initialize Registers
	registers := make(map[string]int)

	// Step 3: Process Instructions
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		reg := parts[0]
		op := parts[1]
		amount, _ := strconv.Atoi(parts[2])
		condReg := parts[4]
		condOp := parts[5]
		condVal, _ := strconv.Atoi(parts[6])

		// Check condition
		cond := false
		switch condOp {
		case ">":
			cond = registers[condReg] > condVal
		case ">=":
			cond = registers[condReg] >= condVal
		case "<":
			cond = registers[condReg] < condVal
		case "<=":
			cond = registers[condReg] <= condVal
		case "==":
			cond = registers[condReg] == condVal
		case "!=":
			cond = registers[condReg] != condVal
		}

		if cond {
			switch op {
			case "inc":
				registers[reg] += amount
			case "dec":
				registers[reg] -= amount
			}
		}
	}

	// Step 4: Find Max Value
	maxValue := 0
	for _, value := range registers {
		if value > maxValue {
			maxValue = value
		}
	}

	fmt.Println(maxValue)
}