package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type ModuleType int

const (
	Broadcaster ModuleType = iota
	FlipFlop
	Conjunction
)

type Pulse struct {
	source      string
	destination string
	high        bool
}

type Module struct {
	name         string
	moduleType   ModuleType
	destinations []string
	state        bool
	memory       map[string]bool
}

func main() {
	modules := parseInput("input.txt")
	lowPulses, highPulses := simulatePulses(modules, 1000)
	fmt.Println(lowPulses * highPulses)
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
		switch {
		case name == "broadcaster":
			moduleType = Broadcaster
		case name[0] == '%':
			moduleType = FlipFlop
			name = name[1:]
		case name[0] == '&':
			moduleType = Conjunction
			name = name[1:]
		}

		modules[name] = &Module{
			name:         name,
			moduleType:   moduleType,
			destinations: destinations,
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

func simulatePulses(modules map[string]*Module, buttonPresses int) (int, int) {
	lowPulses, highPulses := 0, 0
	queue := make([]Pulse, 0)

	for i := 0; i < buttonPresses; i++ {
		queue = append(queue, Pulse{source: "button", destination: "broadcaster", high: false})

		for len(queue) > 0 {
			pulse := queue[0]
			queue = queue[1:]

			if pulse.high {
				highPulses++
			} else {
				lowPulses++
			}

			module, exists := modules[pulse.destination]
			if !exists {
				continue
			}

			switch module.moduleType {
			case Broadcaster:
				for _, dest := range module.destinations {
					queue = append(queue, Pulse{source: module.name, destination: dest, high: pulse.high})
				}
			case FlipFlop:
				if !pulse.high {
					module.state = !module.state
					for _, dest := range module.destinations {
						queue = append(queue, Pulse{source: module.name, destination: dest, high: module.state})
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
					queue = append(queue, Pulse{source: module.name, destination: dest, high: !allHigh})
				}
			}
		}
	}

	return lowPulses, highPulses
}
