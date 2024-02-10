package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Scrambler struct {
	pw []rune
}

func NewScrambler(pw string) *Scrambler {
	return &Scrambler{pw: []rune(pw)}
}

func (s *Scrambler) String() string {
	return string(s.pw)
}

func (s *Scrambler) swapPositions(x, y int) {
	s.pw[x], s.pw[y] = s.pw[y], s.pw[x]
}

func (s *Scrambler) swapLetters(x, y rune) {
	s.swapPositions(strings.IndexRune(string(s.pw), x), strings.IndexRune(string(s.pw), y))
}

func (s *Scrambler) rotate(steps int) {
	length := len(s.pw)
	steps = steps % length
	if steps < 0 {
		steps += length
	}
	s.pw = append(s.pw[length-steps:], s.pw[:length-steps]...)
}

func (s *Scrambler) rotateLetter(x rune) {
	index := strings.IndexRune(string(s.pw), x)
	if index >= 4 {
		index++
	}
	s.rotate(index + 1)
}

func (s *Scrambler) derotateLetter(x rune) {
	index := strings.IndexRune(string(s.pw), x)
	var rot int
	if index%2 == 1 {
		rot = -(index + 1) / 2
	} else if index != 0 {
		rot = (6 - index) / 2
	} else {
		rot = -1
	}
	s.rotate(rot)
}

func (s *Scrambler) reverse(x, y int) {
	for x < y {
		s.pw[x], s.pw[y] = s.pw[y], s.pw[x]
		x++
		y--
	}
}

func (s *Scrambler) move(x, y int) {
	ch := s.pw[x]
	if x < y {
		copy(s.pw[x:], s.pw[x+1:y+1])
	} else {
		copy(s.pw[y+1:x+1], s.pw[y:x])
	}
	s.pw[y] = ch
}

func (s *Scrambler) scramble(instructions []string, direction int) *Scrambler {
	if direction < 0 {
		reverseStrings(instructions)
	}
	for _, instruction := range instructions {
		line := strings.Fields(instruction)
		switch {
		case strings.HasPrefix(instruction, "swap"):
			x, y := line[2], line[len(line)-1]
			if line[1] == "position" {
				xi, _ := strconv.Atoi(x)
				yi, _ := strconv.Atoi(y)
				s.swapPositions(xi, yi)
			} else {
				s.swapLetters(rune(x[0]), rune(y[0]))
			}
		case strings.HasPrefix(instruction, "rotate"):
			if line[1] == "based" {
				if direction > 0 {
					s.rotateLetter(rune(line[len(line)-1][0]))
				} else {
					s.derotateLetter(rune(line[len(line)-1][0]))
				}
			} else {
				x, _ := strconv.Atoi(line[2])
				if line[1] == "left" {
					x = -x
				}
				if direction < 0 {
					x = -x
				}
				s.rotate(x)
			}
		case strings.HasPrefix(instruction, "reverse"):
			x, y := line[2], line[len(line)-1]
			xi, _ := strconv.Atoi(x)
			yi, _ := strconv.Atoi(y)
			s.reverse(xi, yi)
		case strings.HasPrefix(instruction, "move"):
			x, y := line[2], line[len(line)-1]
			xi, _ := strconv.Atoi(x)
			yi, _ := strconv.Atoi(y)
			if direction < 0 {
				xi, yi = yi, xi
			}
			s.move(xi, yi)
		}
	}
	return s
}

func (s *Scrambler) unscramble(instructions []string) *Scrambler {
	return s.scramble(instructions, -1)
}

func reverseStrings(s []string) {
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var instructions []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		instructions = append(instructions, scanner.Text())
	}

	hashed := "fbgdceah"
	scrambler := NewScrambler(hashed)
	result := scrambler.unscramble(instructions)
	fmt.Println(result)
}