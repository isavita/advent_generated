package main

import (
	"fmt"
	"os"
	"strings"
)

type Mirror struct {
	Rows []int
	Cols []int
}

func parseInput(input []string) []Mirror {
	mirrors := []Mirror{}

	mirrorStr := []string{}
	for _, line := range input {
		if line == "" {
			mirrors = append(mirrors, parseMirror(mirrorStr))
			mirrorStr = []string{}
		} else {
			mirrorStr = append(mirrorStr, line)
		}
	}
	mirrors = append(mirrors, parseMirror(mirrorStr))

	return mirrors
}

func parseMirror(mirrorStr []string) Mirror {
	mirror := Mirror{
		Rows: make([]int, len(mirrorStr)),
		Cols: make([]int, len(mirrorStr[0])),
	}

	for y, line := range mirrorStr {
		for x, char := range line {
			mirror.Rows[y] <<= 1
			mirror.Cols[x] <<= 1
			if char == '#' {
				mirror.Rows[y]++
				mirror.Cols[x]++
			}
		}
	}

	return mirror
}

func getMirrorAxis(lines []int) int {
	for i := 1; i < len(lines); i++ {
		isMirror := true

		for j := 0; isMirror && j < min(i, len(lines)-i); j++ {
			if lines[i-1-j] != lines[i+j] {
				isMirror = false
			}
		}

		if isMirror {
			return i
		}
	}

	return 0
}

func getMirrorAxisWithOneSmudge(lines []int) int {
	for i := 1; i < len(lines); i++ {
		isMirror := true
		numSmudges := 0

		for j := 0; isMirror && j < min(i, len(lines)-i); j++ {
			if lines[i-1-j] != lines[i+j] {
				if numSmudges > 0 {
					isMirror = false
				} else {
					dif := lines[i-1-j] ^ lines[i+j]
					isOnlyOneSmudge := (dif & (dif - 1)) == 0
					if isOnlyOneSmudge {
						numSmudges++
					} else {
						isMirror = false
					}
				}
			}
		}

		if isMirror && numSmudges == 1 {
			return i
		}
	}

	return 0
}

func solve(input []string) int {
	mirrors := parseInput(input)

	res := 0
	for _, mirror := range mirrors {
		res += getMirrorAxisWithOneSmudge(mirror.Cols)
		res += getMirrorAxisWithOneSmudge(mirror.Rows) * 100
	}
	return res
}

func readFile(fileName string) []string {
	file, err := os.ReadFile(fileName)
	if err != nil {
		panic(err)
	}

	return strings.Split(strings.TrimSpace(string(file)), "\n")
}

func main() {
	input := readFile("input.txt")
	fmt.Println(solve(input))
}