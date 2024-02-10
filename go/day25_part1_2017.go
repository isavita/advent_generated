package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func parseInput(filePath string) (string, int, map[string]map[int][]interface{}) {
	file, err := os.Open(filePath)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	initialState := string(lines[0][len(lines[0])-2])
	steps, _ := strconv.Atoi(regexp.MustCompile(`\d+`).FindString(lines[1]))

	states := make(map[string]map[int][]interface{})
	for i := 3; i < len(lines); i += 10 {
		state := string(lines[i][len(lines[i])-2])
		value0, _ := strconv.Atoi(string(lines[i+2][len(lines[i+2])-2]))
		move0 := 1
		if strings.HasSuffix(lines[i+3], "left.") {
			move0 = -1
		}
		nextState0 := string(lines[i+4][len(lines[i+4])-2])
		value1, _ := strconv.Atoi(string(lines[i+6][len(lines[i+6])-2]))
		move1 := 1
		if strings.HasSuffix(lines[i+7], "left.") {
			move1 = -1
		}
		nextState1 := string(lines[i+8][len(lines[i+8])-2])
		states[state] = map[int][]interface{}{0: {value0, move0, nextState0}, 1: {value1, move1, nextState1}}
	}
	return initialState, steps, states
}

func runTuringMachine(filePath string) int {
	state, steps, states := parseInput(filePath)
	tape := make(map[int]int)
	cursor := 0
	checksum := 0

	for i := 0; i < steps; i++ {
		value, exists := tape[cursor]
		if !exists {
			value = 0
		}
		newValue := states[state][value][0].(int)
		move := states[state][value][1].(int)
		nextState := states[state][value][2].(string)

		tape[cursor] = newValue
		cursor += move
		state = nextState
	}

	for _, v := range tape {
		checksum += v
	}
	return checksum
}

func main() {
	result := runTuringMachine("input.txt")
	fmt.Println(result)
}