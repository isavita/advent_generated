package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

var (
	Up    = complex(0, -1)
	Down  = complex(0, 1)
	Left  = complex(-1, 0)
	Right = complex(1, 0)
)

func main() {
	data, _ := ioutil.ReadFile("input.txt")
	input := string(data)
	fmt.Printf("%d\n", int(solve(input)))
	fmt.Printf("%d\n", int(solve(scaleUp(input))))
}

func solve(input string) float64 {
	m, steps := parse(input)
	var robot complex128
	for k, v := range m {
		if v == '@' {
			robot = k
			break
		}
	}
	for _, dir := range steps {
		if tryToStep(&m, robot, dir) {
			robot += dir
		}
	}
	var sum float64
	for k, v := range m {
		if v == '[' || v == 'O' {
			sum += float64(real(k)) + 100*float64(imag(k))
		}
	}
	return sum
}

func tryToStep(m *map[complex128]rune, pos, dir complex128) bool {
	orig := copyMap(*m)
	if (*m)[pos] == '.' {
		return true
	} else if (*m)[pos] == 'O' || (*m)[pos] == '@' {
		if tryToStep(m, pos+dir, dir) {
			(*m)[pos+dir] = (*m)[pos]
			(*m)[pos] = '.'
			return true
		}
	} else if (*m)[pos] == ']' {
		if tryToStep(m, pos+Left, dir) {
			return true
		}
	} else if (*m)[pos] == '[' {
		if dir == Left {
			if tryToStep(m, pos+Left, dir) {
				(*m)[pos+Left] = '['
				(*m)[pos] = ']'
				(*m)[pos+Right] = '.'
				return true
			}
		} else if dir == Right {
			if tryToStep(m, pos+2*Right, dir) {
				(*m)[pos] = '.'
				(*m)[pos+Right] = '['
				(*m)[pos+2*Right] = ']'
				return true
			}
		} else {
			if tryToStep(m, pos+dir, dir) && tryToStep(m, pos+Right+dir, dir) {
				(*m)[pos] = '.'
				(*m)[pos+Right] = '.'
				(*m)[pos+dir] = '['
				(*m)[pos+dir+Right] = ']'
				return true
			}
		}
	}
	*m = orig
	return false
}

func scaleUp(input string) string {
	r := strings.ReplaceAll
	s := input
	s = r(s, "#", "##")
	s = r(s, ".", "..")
	s = r(s, "O", "[]")
	s = r(s, "@", "@.")
	return s
}

func parse(input string) (map[complex128]rune, []complex128) {
	blocks := strings.Split(strings.TrimSpace(input), "\n\n")
	lines := strings.Split(blocks[0], "\n")
	m := map[complex128]rune{}
	for y := 0; y < len(lines); y++ {
		for x := 0; x < len(lines[y]); x++ {
			m[complex(float64(x), float64(y))] = rune(lines[y][x])
		}
	}
	var steps []complex128
	for _, ch := range strings.ReplaceAll(blocks[1], "\n", "") {
		switch ch {
		case '^':
			steps = append(steps, Up)
		case '<':
			steps = append(steps, Left)
		case '>':
			steps = append(steps, Right)
		case 'v':
			steps = append(steps, Down)
		}
	}
	return m, steps
}

func copyMap(src map[complex128]rune) map[complex128]rune {
	dst := make(map[complex128]rune, len(src))
	for k, v := range src {
		dst[k] = v
	}
	return dst
}
