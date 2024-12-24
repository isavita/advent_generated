package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

// Gate represents a logic gate with two inputs, an operation, and an output
type Gate struct {
	input1    string
	input2    string
	operation string
	output    string
}

func main() {
	// Open the input file
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening input.txt:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	// Maps and slices to store wire values and gates
	wires := make(map[string]int)
	var gates []Gate

	// Regular expressions to parse lines
	// For initial wire values: e.g., "x00: 1"
	wireRegex := regexp.MustCompile(`^(\w+):\s*([01])$`)
	// For gate definitions: e.g., "x00 AND y00 -> z00"
	gateRegex := regexp.MustCompile(`^(\w+)\s+(AND|OR|XOR)\s+(\w+)\s+->\s+(\w+)$`)

	// First, parse initial wire values until an empty line
	parsingWires := true
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			// Empty line indicates switch to parsing gates
			parsingWires = false
			continue
		}
		if parsingWires {
			// Parse initial wire values
			matches := wireRegex.FindStringSubmatch(line)
			if len(matches) != 3 {
				fmt.Println("Invalid wire definition:", line)
				return
			}
			wireName := matches[1]
			wireValue, err := strconv.Atoi(matches[2])
			if err != nil {
				fmt.Println("Invalid wire value in line:", line)
				return
			}
			wires[wireName] = wireValue
		} else {
			// Parse gate definitions
			matches := gateRegex.FindStringSubmatch(line)
			if len(matches) != 5 {
				fmt.Println("Invalid gate definition:", line)
				return
			}
			input1 := matches[1]
			operation := matches[2]
			input2 := matches[3]
			output := matches[4]
			gate := Gate{
				input1:    input1,
				input2:    input2,
				operation: operation,
				output:    output,
			}
			gates = append(gates, gate)
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading input.txt:", err)
		return
	}

	// Simulate the gates
	remainingGates := gates

	for len(remainingGates) > 0 {
		progress := false
		newRemainingGates := make([]Gate, 0, len(remainingGates))
		for _, gate := range remainingGates {
			val1, ok1 := wires[gate.input1]
			val2, ok2 := wires[gate.input2]
			if ok1 && ok2 {
				// Both inputs are available, evaluate the gate
				var outputVal int
				switch gate.operation {
				case "AND":
					if val1 == 1 && val2 == 1 {
						outputVal = 1
					} else {
						outputVal = 0
					}
				case "OR":
					if val1 == 1 || val2 == 1 {
						outputVal = 1
					} else {
						outputVal = 0
					}
				case "XOR":
					if val1 != val2 {
						outputVal = 1
					} else {
						outputVal = 0
					}
				default:
					fmt.Println("Unknown operation:", gate.operation)
					return
				}
				// Assign the output value to the output wire
				wires[gate.output] = outputVal
				progress = true
			} else {
				// Inputs not yet available, keep the gate for later
				newRemainingGates = append(newRemainingGates, gate)
			}
		}
		if !progress {
			fmt.Println("Cannot evaluate remaining gates due to missing inputs or cyclic dependencies.")
			return
		}
		remainingGates = newRemainingGates
	}

	// Collect wires starting with 'z'
	zWires := make(map[int]int)
	zRegex := regexp.MustCompile(`^z(\d+)$`)
	for wire, val := range wires {
		matches := zRegex.FindStringSubmatch(wire)
		if len(matches) == 2 {
			index, err := strconv.Atoi(matches[1])
			if err != nil {
				fmt.Println("Invalid z wire index in wire:", wire)
				return
			}
			zWires[index] = val
		}
	}

	if len(zWires) == 0 {
		fmt.Println("No wires starting with 'z' found.")
		return
	}

	// Sort the z wire indices
	indices := make([]int, 0, len(zWires))
	for idx := range zWires {
		indices = append(indices, idx)
	}
	sort.Ints(indices)

	// Build the binary string, starting from highest index to lowest
	// to have higher bits on the left and z00 as the least significant bit
	binaryBits := make([]byte, len(indices))
	for i, idx := range indices {
		bit := zWires[idx]
		if bit != 0 && bit != 1 {
			fmt.Printf("Invalid bit value for wire z%02d: %d\n", idx, bit)
			return
		}
		binaryBits[i] = byte('0' + bit)
	}

	// Reverse the slice to have higher indices first
	for i, j := 0, len(binaryBits)-1; i < j; i, j = i+1, j-1 {
		binaryBits[i], binaryBits[j] = binaryBits[j], binaryBits[i]
	}

	binaryString := string(binaryBits)

	// Convert binary string to decimal
	decimalValue, err := strconv.ParseInt(binaryString, 2, 64)
	if err != nil {
		fmt.Println("Error converting binary to decimal:", err)
		return
	}

	// Print the decimal value
	fmt.Println(decimalValue)
}
