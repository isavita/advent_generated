package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type ModuleType int

const (
	Broadcast ModuleType = iota
	FlipFlop
	Conjunction
)

type Module struct {
	name        string
	moduleType  ModuleType
	destinations []string
	state       bool
	memory      map[string]bool
}

type Pulse struct {
	source      string
	destination string
	high        bool
}

func parseInput(filename string) map[string]*Module {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)

	modules := make(map[string]*Module)

	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), " -> ")
		name := parts[0]
		destinations := strings.Split(parts[1], ", ")

		var moduleType ModuleType
		if name == "broadcaster" {
			moduleType = Broadcast
		} else if name[0] == '%' {
			moduleType = FlipFlop
			name = name[1:]
		} else if name[0] == '&' {
			moduleType = Conjunction
			name = name[1:]
		}

		modules[name] = &Module{
			name:         name,
			moduleType:   moduleType,
			destinations: destinations,
			state:        false,
			memory:       make(map[string]bool),
		}
	}

	// Initialize conjunction modules' memory
	for name, module := range modules {
		for _, dest := range module.destinations {
			if destModule, exists := modules[dest]; exists && destModule.moduleType == Conjunction {
				destModule.memory[name] = false
			}
		}
	}

	return modules
}

func pushButton(modules map[string]*Module) (int, int) {
	lowCount, highCount := 1, 0 // Start with 1 low pulse for the button press
	queue := []Pulse{{"button", "broadcaster", false}}

	for len(queue) > 0 {
		pulse := queue[0]
		queue = queue[1:]

		module, exists := modules[pulse.destination]
		if !exists {
			continue
		}

		switch module.moduleType {
		case Broadcast:
			for _, dest := range module.destinations {
				if pulse.high {
					highCount++
				} else {
					lowCount++
				}
				queue = append(queue, Pulse{module.name, dest, pulse.high})
			}
		case FlipFlop:
			if !pulse.high {
				module.state = !module.state
				for _, dest := range module.destinations {
					if module.state {
						highCount++
					} else {
						lowCount++
					}
					queue = append(queue, Pulse{module.name, dest, module.state})
				}
			}
		case Conjunction:
			module.memory[pulse.source] = pulse.high
			allHigh := true
			for _, state := range module.memory {
				if !state {
					allHigh = false
					break
				}
			}
			for _, dest := range module.destinations {
				if allHigh {
					lowCount++
					queue = append(queue, Pulse{module.name, dest, false})
				} else {
					highCount++
					queue = append(queue, Pulse{module.name, dest, true})
				}
			}
		}
	}

	return lowCount, highCount
}

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func lcm(a, b int) int {
	return a * b / gcd(a, b)
}

func findCycle(modules map[string]*Module, target string) int {
	count := 0
	for {
		count++
		lowCount, _ := pushButton(modules)
		if lowCount > 0 && modules[target].state {
			return count
		}
	}
}

func main() {
	modules := parseInput("input.txt")

	// Part 1
	lowTotal, highTotal := 0, 0
	for i := 0; i < 1000; i++ {
		low, high := pushButton(modules)
		lowTotal += low
		highTotal += high
	}
	fmt.Printf("Part 1: %d\n", lowTotal*highTotal)

	// Part 2
	modules = parseInput("input.txt") // Reset modules
	rxInputs := []string{}
	for _, module := range modules {
		for _, dest := range module.destinations {
			if dest == "rx" {
				rxInputs = append(rxInputs, module.name)
			}
		}
	}

	cycles := make([]int, len(rxInputs))
	for i, input := range rxInputs {
		cycles[i] = findCycle(modules, input)
	}

	result := cycles[0]
	for i := 1; i < len(cycles); i++ {
		result = lcm(result, cycles[i])
	}

	fmt.Printf("Part 2: %d\n", result)
}
