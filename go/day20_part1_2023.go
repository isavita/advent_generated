package main

import (
	"fmt"
	"os"
	"strings"
)

type Module struct {
	name         string
	prefix       byte
	destinations []string
	state        bool
	memory       map[string]PulseValue
}

type Pulse struct {
	value    PulseValue
	fromName string
	toName   string
}

type PulseValue int

const (
	Low PulseValue = iota
	High
)

const (
	FlipFlop    byte = '%'
	Conjunction byte = '&'
)

func parseInput(input []string) map[string]*Module {
	prefixes := []byte{FlipFlop, Conjunction}
	modules := make(map[string]*Module, len(input))

	for _, line := range input {
		parts := strings.Split(line, " -> ")

		module := Module{}
		isPrefix := false
		for _, prefix := range prefixes {
			if parts[0][0] == prefix {
				module.prefix = prefix
				module.name = parts[0][1:]
				isPrefix = true
				continue
			}
		}
		if !isPrefix {
			module.name = parts[0]
		}
		module.destinations = strings.Split(parts[1], ", ")
		module.memory = map[string]PulseValue{}

		modules[module.name] = &module
	}

	for _, module := range modules {
		for _, destName := range module.destinations {
			if destModule, ok := modules[destName]; ok && destModule.prefix == Conjunction {
				destModule.memory[module.name] = Low
			}
		}
	}

	return modules
}

func pushButton(modules map[string]*Module, startPulse Pulse, numCycle int) (int, int) {
	cntLow := 0
	cntHigh := 0
	pulseQueue := []Pulse{}

	for i := 0; i < numCycle; i++ {
		pulseQueue = append(pulseQueue, startPulse)

		for len(pulseQueue) > 0 {
			pulse := pulseQueue[0]
			pulseQueue = pulseQueue[1:]

			// fmt.Printf("%s -%s -> %s\n", pulse.fromName, map[PulseValue]string{Low: "low", High: "high"}[pulse.value], pulse.toName)
			if pulse.value == Low {
				cntLow++
			} else {
				cntHigh++
			}

			if _, ok := modules[pulse.toName]; !ok {
				continue
			}

			module := modules[pulse.toName]
			var newPulseValue PulseValue
			switch module.prefix {
			case FlipFlop:
				if pulse.value == Low {
					module.state = !module.state
					if module.state {
						newPulseValue = High
					} else {
						newPulseValue = Low
					}
				} else {
					continue
				}

			case Conjunction:
				module.memory[pulse.fromName] = pulse.value
				isHighForAll := true
				for _, value := range module.memory {
					if value == Low {
						isHighForAll = false
						break
					}
				}

				if isHighForAll {
					newPulseValue = Low
				} else {
					newPulseValue = High
				}

			default:
				newPulseValue = pulse.value
			}

			for _, destName := range module.destinations {
				newPulse := Pulse{
					value:    newPulseValue,
					fromName: pulse.toName,
					toName:   destName,
				}
				pulseQueue = append(pulseQueue, newPulse)
			}
		}
	}

	return cntLow, cntHigh
}

func solve(input []string) int {
	startPulse := Pulse{
		value:    Low,
		fromName: "button",
		toName:   "broadcaster",
	}
	numCycle := 1000

	modules := parseInput(input)

	cntLow, cntHigh := pushButton(modules, startPulse, numCycle)

	return cntLow * cntHigh
}

func readFile(fileName string) []string {
	file, err := os.ReadFile(fileName)
	if err != nil {
		panic(err)
	}

	return strings.Split(strings.TrimSpace(string(file)), "\n")
}

func main() {
	input := readFile("input.txt")
	fmt.Println(solve(input))
}