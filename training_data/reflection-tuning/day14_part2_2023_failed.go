package main

import (
	"fmt"
	"strings"
)

type Platform [][]rune

func (p Platform) String() string {
	var sb strings.Builder
	for _, row := range p {
		sb.WriteString(string(row))
		sb.WriteRune('\n')
	}
	return sb.String()
}

func (p Platform) Copy() Platform {
	newP := make(Platform, len(p))
	for i, row := range p {
		newP[i] = make([]rune, len(row))
		copy(newP[i], row)
	}
	return newP
}

func tiltNorth(p Platform) {
	for col := 0; col < len(p[0]); col++ {
		emptyRow := -1
		for row := 0; row < len(p); row++ {
			switch p[row][col] {
			case 'O':
				if emptyRow != -1 {
					p[emptyRow][col] = 'O'
					p[row][col] = '.'
					emptyRow++
				}
			case '.':
				if emptyRow == -1 {
					emptyRow = row
				}
			case '#':
				emptyRow = -1
			}
		}
	}
}

func tiltSouth(p Platform) {
	for col := 0; col < len(p[0]); col++ {
		emptyRow := -1
		for row := len(p) - 1; row >= 0; row-- {
			switch p[row][col] {
			case 'O':
				if emptyRow != -1 {
					p[emptyRow][col] = 'O'
					p[row][col] = '.'
					emptyRow--
				}
			case '.':
				if emptyRow == -1 {
					emptyRow = row
				}
			case '#':
				emptyRow = -1
			}
		}
	}
}

func tiltWest(p Platform) {
	for row := 0; row < len(p); row++ {
		emptyCol := -1
		for col := 0; col < len(p[0]); col++ {
			switch p[row][col] {
			case 'O':
				if emptyCol != -1 {
					p[row][emptyCol] = 'O'
					p[row][col] = '.'
					emptyCol++
				}
			case '.':
				if emptyCol == -1 {
					emptyCol = col
				}
			case '#':
				emptyCol = -1
			}
		}
	}
}

func tiltEast(p Platform) {
	for row := 0; row < len(p); row++ {
		emptyCol := -1
		for col := len(p[0]) - 1; col >= 0; col-- {
			switch p[row][col] {
			case 'O':
				if emptyCol != -1 {
					p[row][emptyCol] = 'O'
					p[row][col] = '.'
					emptyCol--
				}
			case '.':
				if emptyCol == -1 {
					emptyCol = col
				}
			case '#':
				emptyCol = -1
			}
		}
	}
}

func spinCycle(p Platform) {
	tiltNorth(p)
	tiltWest(p)
	tiltSouth(p)
	tiltEast(p)
}

func calculateLoad(p Platform) int {
	load := 0
	for row := 0; row < len(p); row++ {
		for col := 0; col < len(p[0]); col++ {
			if p[row][col] == 'O' {
				load += len(p) - row
			}
		}
	}
	return load
}

func solvePart2(p Platform) int {
	seen := make(map[string]int)
	states := []Platform{p.Copy()}

	cycleStart := 0
	cycleLength := 0

	for i := 0; i < 1000000000; i++ {
		spinCycle(p)
		state := p.String()
		if prev, ok := seen[state]; ok {
			cycleStart = prev
			cycleLength = i - prev
			break
		}
		seen[state] = i
		states = append(states, p.Copy())
	}

	remainingCycles := (1000000000 - cycleStart) % cycleLength
	finalState := states[cycleStart+remainingCycles]

	return calculateLoad(finalState)
}

func main() {
	input := `O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....`

	platform := Platform{}
	for _, line := range strings.Split(input, "\n") {
		platform = append(platform, []rune(line))
	}

	result := solvePart2(platform)
	fmt.Println(result)
}
