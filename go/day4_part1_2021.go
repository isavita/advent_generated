package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type BingoBoard struct {
	numbers [][]int
	marked  [][]bool
}

func (board *BingoBoard) mark(number int) {
	for i, row := range board.numbers {
		for j, n := range row {
			if n == number {
				board.marked[i][j] = true
			}
		}
	}
}

func (board *BingoBoard) hasWon() bool {
	for i := range board.marked {
		if isRowMarked(board.marked[i]) || isColumnMarked(board.marked, i) {
			return true
		}
	}
	return false
}

func (board *BingoBoard) unmarkedSum() int {
	sum := 0
	for i, row := range board.numbers {
		for j, n := range row {
			if !board.marked[i][j] {
				sum += n
			}
		}
	}
	return sum
}

func isRowMarked(row []bool) bool {
	for _, marked := range row {
		if !marked {
			return false
		}
	}
	return true
}

func isColumnMarked(marked [][]bool, column int) bool {
	for _, row := range marked {
		if !row[column] {
			return false
		}
	}
	return true
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	numbers := strings.Split(scanner.Text(), ",")
	var boards []*BingoBoard
	for scanner.Scan() {
		board := &BingoBoard{
			numbers: make([][]int, 5),
			marked:  make([][]bool, 5),
		}
		for i := range board.numbers {
			scanner.Scan()
			line := strings.Fields(scanner.Text())
			board.numbers[i] = make([]int, 5)
			board.marked[i] = make([]bool, 5)
			for j, n := range line {
				board.numbers[i][j], _ = strconv.Atoi(n)
			}
		}
		boards = append(boards, board)
	}

	var winningBoard *BingoBoard
	var winningNumber int
	for _, number := range numbers {
		n, _ := strconv.Atoi(number)
		for _, board := range boards {
			board.mark(n)
			if board.hasWon() {
				winningBoard = board
				winningNumber = n
				break
			}
		}
		if winningBoard != nil {
			break
		}
	}

	fmt.Println(winningBoard.unmarkedSum() * winningNumber)
}