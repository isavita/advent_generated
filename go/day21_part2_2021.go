package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(file))
	result := solve(input)
	fmt.Println(result)
}

func solve(input string) int64 {
	positions := parseInput(input)
	w1, w2 := play([2]int{positions[0], positions[1]}, [2]int{}, 3, true, map[string][2]int64{})

	if w1 > w2 {
		return w1
	}
	return w2
}

func play(positions, scores [2]int, rollsLeftInTurn int, isPlayer1sTurn bool, memo map[string][2]int64) (wins1, wins2 int64) {
	key := fmt.Sprint(positions, scores, rollsLeftInTurn, isPlayer1sTurn)
	if res, ok := memo[key]; ok {
		return res[0], res[1]
	}

	playerIndex := 1
	if isPlayer1sTurn {
		playerIndex = 0
	}

	scoresCopy := [2]int{scores[0], scores[1]}
	if rollsLeftInTurn == 0 {
		scoresCopy[playerIndex] += positions[playerIndex]

		if scoresCopy[playerIndex] >= 21 {
			if playerIndex == 0 {
				return 1, 0
			}
			return 0, 1
		}

		isPlayer1sTurn = !isPlayer1sTurn
		rollsLeftInTurn = 3

		playerIndex++
		playerIndex %= 2
	}

	for roll := 1; roll <= 3; roll++ {

		positionsCopy := [2]int{positions[0], positions[1]}
		positionsCopy[playerIndex] += roll
		if positionsCopy[playerIndex] > 10 {
			positionsCopy[playerIndex] -= 10
		}
		r1, r2 := play(positionsCopy, scoresCopy, rollsLeftInTurn-1, isPlayer1sTurn, memo)
		wins1 += r1
		wins2 += r2
	}

	memo[key] = [2]int64{wins1, wins2}
	return wins1, wins2
}

func parseInput(input string) (ans []int) {
	for _, line := range strings.Split(input, "\n") {

		var player, startingPosition int
		fmt.Sscanf(line, "Player %d starting position: %d", &player, &startingPosition)
		ans = append(ans, startingPosition)
	}
	return ans
}