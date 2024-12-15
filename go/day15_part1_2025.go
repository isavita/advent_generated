package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"strings"
)

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	sc := bufio.NewScanner(f)
	var grid []string
	var movesBuffer bytes.Buffer
	readingMap := true

	for sc.Scan() {
		line := sc.Text()
		if readingMap {
			if strings.Contains(line, "#") {
				grid = append(grid, line)
			} else {
				readingMap = false
				movesBuffer.WriteString(line)
			}
		} else {
			movesBuffer.WriteString(line)
		}
	}

	moves := movesBuffer.String()

	// Convert grid to mutable structure
	runes := make([][]rune, len(grid))
	for i, row := range grid {
		runes[i] = []rune(row)
	}

	var robotR, robotC int
	for r := 0; r < len(runes); r++ {
		for c := 0; c < len(runes[r]); c++ {
			if runes[r][c] == '@' {
				robotR, robotC = r, c
			}
		}
	}

	dirs := map[rune][2]int{
		'^': {-1, 0},
		'v': {1, 0},
		'<': {0, -1},
		'>': {0, 1},
	}

	for _, move := range moves {
		d := dirs[move]
		nr, nc := robotR+d[0], robotC+d[1]
		if runes[nr][nc] == '#' {
			continue
		} else if runes[nr][nc] == 'O' {
			if !pushBoxes(runes, nr, nc, d[0], d[1]) {
				continue
			}
		}
		if runes[nr][nc] == '.' || runes[nr][nc] == 'O' {
			runes[robotR][robotC], runes[nr][nc] = '.', '@'
			robotR, robotC = nr, nc
		}
	}

	sum := 0
	for r := 0; r < len(runes); r++ {
		for c := 0; c < len(runes[r]); c++ {
			if runes[r][c] == 'O' {
				sum += r*100 + c
			}
		}
	}

	fmt.Println(sum)
}

func pushBoxes(runes [][]rune, r, c, dr, dc int) bool {
	nr, nc := r+dr, c+dc
	if runes[nr][nc] == '#' {
		return false
	}
	if runes[nr][nc] == 'O' {
		if !pushBoxes(runes, nr, nc, dr, dc) {
			return false
		}
	}
	if runes[nr][nc] == '.' {
		runes[nr][nc] = 'O'
		runes[r][c] = '.'
		return true
	}
	return false
}
