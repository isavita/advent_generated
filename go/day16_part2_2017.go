package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	moves := strings.Split(scanner.Text(), ",")

	programs := []rune("abcdefghijklmnop")
	initial := string(programs)
	cycleLen := 0

	for i := 0; i < 1000000000; i++ {
		for _, move := range moves {
			switch move[0] {
			case 's':
				x, _ := strconv.Atoi(move[1:])
				spin(programs, x)
			case 'x':
				positions := strings.Split(move[1:], "/")
				A, _ := strconv.Atoi(positions[0])
				B, _ := strconv.Atoi(positions[1])
				exchange(programs, A, B)
			case 'p':
				positions := strings.Split(move[1:], "/")
				A := rune(positions[0][0])
				B := rune(positions[1][0])
				partner(programs, A, B)
			}
		}

		if string(programs) == initial {
			cycleLen = i + 1
			break
		}
	}

	// Reset to initial state
	programs = []rune(initial)

	// Perform the dance (10^9 mod cycleLen) times
	for i := 0; i < 1000000000%cycleLen; i++ {
		for _, move := range moves {
			switch move[0] {
			case 's':
				x, _ := strconv.Atoi(move[1:])
				spin(programs, x)
			case 'x':
				positions := strings.Split(move[1:], "/")
				A, _ := strconv.Atoi(positions[0])
				B, _ := strconv.Atoi(positions[1])
				exchange(programs, A, B)
			case 'p':
				positions := strings.Split(move[1:], "/")
				A := rune(positions[0][0])
				B := rune(positions[1][0])
				partner(programs, A, B)
			}
		}
	}

	fmt.Println(string(programs))
}

func spin(programs []rune, x int) {
	n := len(programs)
	temp := make([]rune, n)
	copy(temp, programs)

	for i := 0; i < n; i++ {
		programs[(i+x)%n] = temp[i]
	}
}

func exchange(programs []rune, A int, B int) {
	programs[A], programs[B] = programs[B], programs[A]
}

func partner(programs []rune, A rune, B rune) {
	var indexA, indexB int
	for i, p := range programs {
		if p == A {
			indexA = i
		}
		if p == B {
			indexB = i
		}
	}
	exchange(programs, indexA, indexB)
}