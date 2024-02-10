package main

import (
	"bufio"
	"fmt"
	"os"
	"reflect"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type FlipFlop struct {
	name       string
	moduleType int
	connectsTo []string
	state      bool
}

type Conjunction struct {
	name       string
	moduleType int
	connectsTo []string
	watches    map[string]bool
}

type Broadcaster struct {
	name       string
	moduleType int
	connectsTo []string
}

const (
	BROADCASTER = iota
	FLIP_FLOP
	CONJUNCTION
)

func handleLine(line string, connections map[string]interface{}) {
	if strings.Contains(line, "broadcaster") {
		var module Broadcaster
		module.moduleType = BROADCASTER
		str := strings.Split(line, " -> ")
		module.name = str[0]
		module.connectsTo = append(module.connectsTo, strings.Split(str[1], ", ")...)
		connections[module.name] = module

	} else if strings.Contains(line, "%") {
		var module FlipFlop
		module.moduleType = FLIP_FLOP
		str := strings.Split(line, " -> ")
		module.name = str[0][1:]
		module.connectsTo = append(module.connectsTo, strings.Split(str[1], ", ")...)
		module.state = false
		connections[module.name] = module

	} else {
		var module Conjunction
		module.moduleType = CONJUNCTION
		str := strings.Split(line, " -> ")
		module.name = str[0][1:]
		module.connectsTo = append(module.connectsTo, strings.Split(str[1], ", ")...)
		module.watches = make(map[string]bool)
		connections[module.name] = module
	}
}

func completeWatches(connections map[string]interface{}) {
	for _, module := range connections {

		switch module := module.(type) {
		case Conjunction:
			conj := module

			for _, module2 := range connections {
				switch module2 := module2.(type) {
				case FlipFlop:
					for _, name := range module2.connectsTo {
						if strings.Compare(name, conj.name) == 0 {
							conj.watches[module2.name] = false
						}
					}
				case Conjunction:
					for _, name := range module2.connectsTo {
						if strings.Compare(name, conj.name) == 0 {
							conj.watches[module2.name] = false
						}
					}
				}
			}
			connections[conj.name] = conj
		}
	}
}

type State struct {
	from  string
	name  string
	pulse bool
}

func simulatePress(connections *map[string]interface{}, loops map[string]int, pressNumber int) ([2]int, bool) {
	queue := make([]State, 0)

	originalConnections := make(map[string]interface{})
	for k, v := range *connections {
		originalConnections[k] = v
	}

	queue = append(queue, State{"button", "broadcaster", false})
	pulses := [2]int{1, 0}

	steps := 0
	found := false
	for len(queue) > 0 {
		currState := queue[0]
		queue = queue[1:]

		module := (*connections)[currState.name]

		if currState.name == "out" {
			continue
		}

		if currState.name == "rx" && !currState.pulse {

			found = true
		}

		pulse := currState.pulse

		switch module := module.(type) {
		case Broadcaster:

			for _, name := range module.connectsTo {
				queue = append(queue, State{module.name, name, pulse})
				if pulse {
					pulses[1]++
				} else {
					pulses[0]++
				}
			}
			(*connections)[currState.name] = module
		case FlipFlop:

			if !pulse {
				module.state = !module.state
				for _, name := range module.connectsTo {
					(*connections)[currState.name] = module
					queue = append(queue, State{module.name, name, module.state})
					if module.state {
						pulses[1]++
					} else {
						pulses[0]++
					}
				}
			}
			(*connections)[currState.name] = module
		case Conjunction:

			module.watches[currState.from] = pulse
			(*connections)[currState.name] = module

			allTrue := true
			for _, state := range module.watches {
				if !state {
					allTrue = false
					break
				}
			}

			for _, name := range module.connectsTo {
				queue = append(queue, State{module.name, name, !allTrue})
				if !allTrue {
					pulses[1]++
				} else {
					pulses[0]++
				}
			}
			(*connections)[currState.name] = module

			currLoop, ok := loops[currState.name]
			if ok && !allTrue && currLoop == -1 {
				loops[currState.name] = pressNumber
			}
		}

		steps++
	}
	return pulses, found
}

func sumHistory(hist [][2]int) int {
	var sum [2]int
	for _, pulses := range hist {
		sum[0] += pulses[0]
		sum[1] += pulses[1]
	}
	return sum[0] * sum[1]
}

func connectsTo(from, to string, connections map[string]interface{}) bool {
	switch module := connections[from].(type) {
	case Broadcaster:
		for _, name := range module.connectsTo {
			if strings.Compare(name, to) == 0 {
				return true
			}
		}
	case FlipFlop:
		for _, name := range module.connectsTo {
			if strings.Compare(name, to) == 0 {
				return true
			}
		}
	case Conjunction:
		for _, name := range module.connectsTo {
			if strings.Compare(name, to) == 0 {
				return true
			}
		}
	}
	return false
}

func copyConnections(connections map[string]interface{}) map[string]interface{} {
	copy := make(map[string]interface{})
	for k, v := range connections {
		copy[k] = v
	}
	return copy
}

func includedInHistory(hist []map[string]interface{}, connections map[string]interface{}) int {
	for i, histConnections := range hist {
		if len(histConnections) != len(connections) {
			continue
		}
		if reflect.DeepEqual(histConnections, connections) {
			return i
		}
	}
	return -1
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var sum = 0
	connections := make(map[string]interface{})

	for scanner.Scan() {
		line := scanner.Text()
		handleLine(line, connections)
	}

	completeWatches(connections)

	pxPrev := make([]string, 0)
	for k := range connections {
		if connectsTo(k, "rx", connections) {
			pxPrev = append(pxPrev, k)
		}
	}

	if !(len(pxPrev) == 1) {
		panic("Error: more than one pxPrev")
	}

	var conj Conjunction
	switch connections[pxPrev[0]].(type) {
	case Conjunction:
		conj = connections[pxPrev[0]].(Conjunction)
	default:
		panic("Error: pxPrev is not a conjunction")
	}

	loopLengths := make(map[string]int)
	for name := range conj.watches {
		loopLengths[name] = -1
	}

	pressNumber := 0
	for {
		pressNumber++
		_, found := simulatePress(&connections, loopLengths, pressNumber)
		if found {
			break
		}
		complete := true
		for _, length := range loopLengths {
			if length == -1 {
				complete = false
				break
			}
		}
		if complete {
			break
		}
	}

	sum = 1
	for _, length := range loopLengths {
		sum *= length
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
	}

	fmt.Printf("%d\n", sum)
}