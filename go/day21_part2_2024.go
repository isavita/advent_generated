package main

import (
	"fmt"
	"os"
	"strings"
)

type Position struct {
	i, j int
}

type CacheKey struct {
	pos      Position
	ch       byte
	padType  string
}

type SolveKey struct {
	code      string
	robots    int
	maxRobots int
}

// Cache maps
var positionCache = make(map[string]Position)
var okCache = make(map[string]bool)
var moveCache = make(map[CacheKey]string)
var solveCache = make(map[SolveKey]int)

func findPosition(mat []string, ch byte) Position {
	// Check cache first
	key := string(ch) + strings.Join(mat, "")
	if pos, exists := positionCache[key]; exists {
		return pos
	}

	for i := range mat {
		for j := range mat[i] {
			if mat[i][j] == ch {
				pos := Position{i, j}
				positionCache[key] = pos
				return pos
			}
		}
	}
	return Position{-1, -1}
}

func ok(mat []string, st Position, seq string) bool {
	key := fmt.Sprintf("%d,%d,%s,%s", st.i, st.j, seq, strings.Join(mat, ""))
	if result, exists := okCache[key]; exists {
		return result
	}

	curr := st
	for i := 0; i < len(seq); i++ {
		if mat[curr.i][curr.j] == ' ' {
			okCache[key] = false
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
			okCache[key] = false
			return false
		}
	}
	
	okCache[key] = true
	return true
}

func generateMoves(position Position, objective byte, pad []string) string {
	key := CacheKey{position, objective, strings.Join(pad, "")}
	if moves, exists := moveCache[key]; exists {
		return moves
	}

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

	result := ret.String()
	if !ok(pad, position, result) {
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
		result = ret.String()
	}
	
	moveCache[key] = result
	return result
}

func solve(code string, robots int, keyPad []string, robotPad []string, maxRobots int) int {
	key := SolveKey{code, robots, maxRobots}
	if result, exists := solveCache[key]; exists {
		return result
	}

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

	solveCache[key] = ret
	return ret
}

func main() {
	content, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	maxRobots := 26 // 25 robots with directional keypads + 1 robot with numeric keypad
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
