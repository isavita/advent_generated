package main

import (
	"fmt"
	"os"
	"strconv"
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

func solve(input string) int {
	nums, boards := parseInput(input)

	lastWinningScore := -1
	alreadyWon := map[int]bool{}
	for _, n := range nums {
		for bi, b := range boards {
			if alreadyWon[bi] {
				continue
			}
			didWin := b.PickNum(n)
			if didWin {

				lastWinningScore = b.Score() * n

				alreadyWon[bi] = true
			}
		}
	}

	return lastWinningScore

}

type BoardState struct {
	board  [][]int
	picked [][]bool
}

func NewBoardState(board [][]int) BoardState {
	picked := make([][]bool, len(board))
	for i := range picked {
		picked[i] = make([]bool, len(board[0]))
	}
	return BoardState{
		board:  board,
		picked: picked,
	}
}

func (b *BoardState) PickNum(num int) bool {
	for r, rows := range b.board {
		for c, v := range rows {
			if v == num {
				b.picked[r][c] = true
			}
		}
	}

	for i := 0; i < len(b.board); i++ {
		isFullRow, isFullCol := true, true

		for j := 0; j < len(b.board); j++ {
			if !b.picked[i][j] {
				isFullRow = false
			}

			if !b.picked[j][i] {
				isFullCol = false
			}
		}
		if isFullRow || isFullCol {

			return true
		}
	}

	return false
}

func (b *BoardState) Score() int {
	var score int

	for r, rows := range b.board {
		for c, v := range rows {
			if !b.picked[r][c] {
				score += v
			}
		}
	}

	return score
}

func parseInput(input string) (nums []int, boards []BoardState) {
	lines := strings.Split(input, "\n\n")

	for _, v := range strings.Split(lines[0], ",") {
		nums = append(nums, toInt(v))
	}

	for _, grid := range lines[1:] {
		b := [][]int{}
		for _, line := range strings.Split(grid, "\n") {
			line = strings.ReplaceAll(line, "  ", " ")
			for line[0] == ' ' {
				line = line[1:]
			}
			parts := strings.Split(line, " ")

			row := []int{}
			for _, p := range parts {
				row = append(row, toInt(p))
			}
			b = append(b, row)
		}

		boards = append(boards, NewBoardState(b))
	}
	return nums, boards
}

func toInt(s string) int {
	n, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return n
}