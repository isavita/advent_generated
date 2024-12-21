package main

import (
	"fmt"
	"os"
	"strings"
)

type Position struct {
	i, j int
}

func findPosition(mat []string, ch byte) Position {
	for i := range mat {
		for j := range mat[i] {
			if mat[i][j] == ch {
				return Position{i, j}
			}
		}
	}
	return Position{-1, -1}
}

func ok(mat []string, st Position, seq string) bool {
	curr := st
	for i := 0; i < len(seq); i++ {
		if mat[curr.i][curr.j] == ' ' {
			return false
		}
		ch := seq[i]
		switch ch {
		case '^':
			curr.i--
		case 'v':
			curr.i++
		case '<':
			curr.j--
		case '>':
			curr.j++
		}
		
		if curr.i < 0 || curr.i >= len(mat) || curr.j < 0 || curr.j >= len(mat[0]) {
			return false
		}
	}
	return true
}

func generateMoves(position Position, objective byte, pad []string) string {
	objPos := findPosition(pad, objective)
	
	ret := strings.Builder{}
	// First attempt
	if position.j > objPos.j {
		ret.WriteString(strings.Repeat("<", position.j-objPos.j))
	}
	if position.i > objPos.i {
		ret.WriteString(strings.Repeat("^", position.i-objPos.i))
	}
	if position.i < objPos.i {
		ret.WriteString(strings.Repeat("v", objPos.i-position.i))
	}
	if position.j < objPos.j {
		ret.WriteString(strings.Repeat(">", objPos.j-position.j))
	}

	// If first attempt fails, try alternative path
	if !ok(pad, position, ret.String()) {
		ret.Reset()
		if position.j < objPos.j {
			ret.WriteString(strings.Repeat(">", objPos.j-position.j))
		}
		if position.i > objPos.i {
			ret.WriteString(strings.Repeat("^", position.i-objPos.i))
		}
		if position.i < objPos.i {
			ret.WriteString(strings.Repeat("v", objPos.i-position.i))
		}
		if position.j > objPos.j {
			ret.WriteString(strings.Repeat("<", position.j-objPos.j))
		}
	}
	
	return ret.String()
}

func solve(code string, robots int, keyPad []string, robotPad []string, maxRobots int) int {
	if robots <= 0 {
		return len(code)
	}

	ret := 0
	posi, posj := 3, 2
	if robots != maxRobots {
		posi = 0
	}

	var moves string
	for i := 0; i < len(code); i++ {
		ch := code[i]
		if robots == maxRobots {
			moves = generateMoves(Position{posi, posj}, ch, keyPad)
			pos := findPosition(keyPad, ch)
			posi, posj = pos.i, pos.j
		} else {
			moves = generateMoves(Position{posi, posj}, ch, robotPad)
			pos := findPosition(robotPad, ch)
			posi, posj = pos.i, pos.j
		}
		ret += solve(moves+"A", robots-1, keyPad, robotPad, maxRobots)
	}
	return ret
}

func main() {
	content, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	maxRobots := 3
	keyPad := []string{
		"789",
		"456",
		"123",
		" 0A",
	}
	robotPad := []string{
		" ^A",
		"<v>",
	}

	ret := 0
	codes := strings.Split(strings.TrimSpace(string(content)), "\n")
	
	for _, code := range codes {
		code = strings.TrimSpace(code)
		if code == "" {
			continue
		}

		numericPart := 0
		for i := 0; i < len(code); i++ {
			if code[i] >= '0' && code[i] <= '9' {
				numericPart = numericPart*10 + int(code[i]-'0')
			}
		}

		sv := solve(code, maxRobots, keyPad, robotPad, maxRobots)
		ret += sv * numericPart
	}

	fmt.Println(ret)
}
