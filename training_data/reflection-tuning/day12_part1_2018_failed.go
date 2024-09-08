package main

import (
	"bufio"
	"fmt"
	"strings"
)

func main() {
	input := `initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #`

	scanner := bufio.NewScanner(strings.NewReader(input))
	scanner.Scan()
	initialState := strings.TrimPrefix(scanner.Text(), "initial state: ")

	rules := make(map[string]byte)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		parts := strings.Split(line, " => ")
		rules[parts[0]] = parts[1][0]
	}

	state := initialState
	leftmostIndex := 0

	for gen := 0; gen < 20; gen++ {
		state = "...." + state + "...."
		leftmostIndex -= 2

		nextState := strings.Repeat(".", len(state))
		for i := 2; i < len(state)-2; i++ {
			pattern := state[i-2 : i+3]
			if newState, exists := rules[pattern]; exists && newState == '#' {
				nextState = nextState[:i] + "#" + nextState[i+1:]
			}
		}

		state = strings.Trim(nextState, ".")
		leftmostIndex += strings.IndexByte(nextState, '#')
	}

	sum := 0
	for i, plant := range state {
		if plant == '#' {
			sum += i + leftmostIndex
		}
	}

	fmt.Println(sum)
}
