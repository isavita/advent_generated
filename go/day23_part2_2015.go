package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	instructions := strings.Split(strings.TrimSpace(string(data)), "\n")

	// Initialize the registers with register a starting at 1
	registers := map[string]int{"a": 1, "b": 0}

	// Execute the instructions
	for i := 0; i < len(instructions); i++ {
		parts := strings.Fields(instructions[i])

		switch parts[0] {
		case "hlf":
			registers[parts[1]] /= 2
		case "tpl":
			registers[parts[1]] *= 3
		case "inc":
			registers[parts[1]]++
		case "jmp":
			offset, _ := strconv.Atoi(parts[1])
			i += offset - 1
		case "jie":
			if registers[parts[1][:1]]%2 == 0 {
				offset, _ := strconv.Atoi(parts[2])
				i += offset - 1
			}
		case "jio":
			if registers[parts[1][:1]] == 1 {
				offset, _ := strconv.Atoi(parts[2])
				i += offset - 1
			}
		default:
			log.Fatalf("Unknown instruction: %s", parts[0])
		}
	}

	fmt.Printf("%d\n", registers["b"])
}