package main

import (
	"fmt"
	"os"
	"sort"
	"strings"
)

// Gate represents a logical gate with inputs and operation
type Gate struct {
	a, op, b string
}

// parse reads and parses the input file focusing on gates
func parse(input string) []struct{ gate Gate; output string } {
	parts := strings.Split(input, "\n\n")
	if len(parts) != 2 {
		return nil
	}

	var gates []struct {
		gate   Gate
		output string
	}
	for _, line := range strings.Split(parts[1], "\n") {
		if line == "" {
			continue
		}
		parts := strings.Split(line, " -> ")
		if len(parts) != 2 {
			continue
		}
		gateParts := strings.Split(parts[0], " ")
		if len(gateParts) != 3 {
			continue
		}
		gates = append(gates, struct {
			gate   Gate
			output string
		}{
			gate:   Gate{gateParts[0], gateParts[1], gateParts[2]},
			output: parts[1],
		})
	}
	return gates
}

// createLookups creates forward and reverse lookups for gates
func createLookups(gates []struct{ gate Gate; output string }) (map[string]Gate, map[string]string) {
	lookup := make(map[string]Gate)
	reverseLookup := make(map[string]string)

	for _, g := range gates {
		lookup[g.output] = g.gate
		// Create unique key for reverse lookup
		inputs := []string{g.gate.a, g.gate.b}
		sort.Strings(inputs)
		key := fmt.Sprintf("%s_%s_%s", inputs[0], g.gate.op, inputs[1])
		reverseLookup[key] = g.output
	}
	return lookup, reverseLookup
}

// swap performs the gate output swapping operation
func swap(pairs *[][2]string, gates []struct{ gate Gate; output string }, a, b string) {
	*pairs = append(*pairs, [2]string{a, b})
	for i := range gates {
		if gates[i].output == a {
			gates[i].output = b
		} else if gates[i].output == b {
			gates[i].output = a
		}
	}
}

// getReverseLookupKey creates a consistent key for reverse lookup
func getReverseLookupKey(a, op, b string) string {
	inputs := []string{a, b}
	sort.Strings(inputs)
	return fmt.Sprintf("%s_%s_%s", inputs[0], op, inputs[1])
}

func solution(gates []struct{ gate Gate; output string }) string {
	var pairs [][2]string
	numZ := 0
	for _, g := range gates {
		if strings.HasPrefix(g.output, "z") {
			numZ++
		}
	}

	for len(pairs) < 4 {
		adder := ""
		carry := ""
		lookup, reverseLookup := createLookups(gates)

		for i := 0; i < numZ; i++ {
			xi := fmt.Sprintf("x%02d", i)
			yi := fmt.Sprintf("y%02d", i)
			zi := fmt.Sprintf("z%02d", i)

			if i == 0 {
				adder = reverseLookup[getReverseLookupKey(xi, "XOR", yi)]
				carry = reverseLookup[getReverseLookupKey(xi, "AND", yi)]
			} else {
				bit := reverseLookup[getReverseLookupKey(xi, "XOR", yi)]
				if bit != "" {
					adder = reverseLookup[getReverseLookupKey(bit, "XOR", carry)]
					if adder != "" {
						c1 := reverseLookup[getReverseLookupKey(xi, "AND", yi)]
						c2 := reverseLookup[getReverseLookupKey(bit, "AND", carry)]
						carry = reverseLookup[getReverseLookupKey(c1, "OR", c2)]
					}
				}
			}

			if adder == "" {
				gate := lookup[zi]
				bitKey := getReverseLookupKey(xi, "XOR", yi)
				bit := reverseLookup[bitKey]
				if reverseLookup[getReverseLookupKey(gate.a, "XOR", carry)] != "" {
					swap(&pairs, gates, bit, gate.a)
					break
				} else if reverseLookup[getReverseLookupKey(gate.b, "XOR", carry)] != "" {
					swap(&pairs, gates, bit, gate.b)
					break
				}
			} else if adder != zi {
				swap(&pairs, gates, adder, zi)
				break
			}
		}
	}

	// Convert pairs to result string
	var result []string
	for _, pair := range pairs {
		result = append(result, pair[0], pair[1])
	}
	sort.Strings(result)
	return strings.Join(result, ",")
}

func main() {
	// Read input from file
	input, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading input file:", err)
		return
	}

	// Parse input and solve
	gates := parse(string(input))
	if gates == nil {
		fmt.Println("Error parsing input")
		return
	}

	fmt.Println(solution(gates))
}
