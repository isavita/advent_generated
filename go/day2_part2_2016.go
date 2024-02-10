package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var instructions []string
	for scanner.Scan() {
		instructions = append(instructions, scanner.Text())
	}

	code := getBathroomCode(instructions)
	fmt.Println(code)
}

func getBathroomCode(instructions []string) string {
	keypad := map[string]map[rune]string{
		"1": {'D': "3"},
		"2": {'R': "3", 'D': "6"},
		"3": {'U': "1", 'R': "4", 'D': "7", 'L': "2"},
		"4": {'L': "3", 'D': "8"},
		"5": {'R': "6"},
		"6": {'U': "2", 'R': "7", 'D': "A", 'L': "5"},
		"7": {'U': "3", 'R': "8", 'D': "B", 'L': "6"},
		"8": {'U': "4", 'R': "9", 'D': "C", 'L': "7"},
		"9": {'L': "8"},
		"A": {'U': "6", 'R': "B"},
		"B": {'U': "7", 'R': "C", 'D': "D", 'L': "A"},
		"C": {'U': "8", 'L': "B"},
		"D": {'U': "B"},
	}
	position := "5" // Start at '5'
	var code string

	for _, instruction := range instructions {
		for _, move := range instruction {
			if nextPos, ok := keypad[position][move]; ok {
				position = nextPos
			}
		}
		code += position
	}

	return code
}