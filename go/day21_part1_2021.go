package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		os.Exit(1)
	}

	lines := strings.Split(string(data), "\n")
	player1Start, _ := strconv.Atoi(strings.TrimSpace(lines[0][28:]))
	player2Start, _ := strconv.Atoi(strings.TrimSpace(lines[1][28:]))
	player1Pos := player1Start
	player2Pos := player2Start

	player1Score := 0
	player2Score := 0

	dieRoll := 1
	rollCount := 0

	for {
		// Player 1
		rolls := dieRoll%100 + (dieRoll+1)%100 + (dieRoll+2)%100
		rollCount += 3
		dieRoll += 3

		player1Pos = (player1Pos+rolls-1)%10 + 1
		player1Score += player1Pos

		if player1Score >= 1000 {
			fmt.Println("Result:", player2Score*rollCount)
			break
		}

		// Player 2
		rolls = dieRoll%100 + (dieRoll+1)%100 + (dieRoll+2)%100
		rollCount += 3
		dieRoll += 3

		player2Pos = (player2Pos+rolls-1)%10 + 1
		player2Score += player2Pos

		if player2Score >= 1000 {
			fmt.Println(player1Score * rollCount)
			break
		}
	}
}