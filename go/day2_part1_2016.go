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
	keypad := [][]int{
		{1, 2, 3},
		{4, 5, 6},
		{7, 8, 9},
	}
	x, y := 1, 1 // Start at '5'
	var code string

	for _, instruction := range instructions {
		for _, move := range instruction {
			switch move {
			case 'U':
				if x > 0 {
					x--
				}
			case 'D':
				if x < 2 {
					x++
				}
			case 'L':
				if y > 0 {
					y--
				}
			case 'R':
				if y < 2 {
					y++
				}
			}
		}
		code += fmt.Sprint(keypad[x][y])
	}

	return code
}