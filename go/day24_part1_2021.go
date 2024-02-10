package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	var k, l, m []int
	for i, line := range strings.Split(readAll("input.txt"), "\n") {
		var v int
		switch i % 18 {
		case 4:
			fmt.Sscanf(line, "div z %d", &v)
			l = append(l, v)
		case 5:
			fmt.Sscanf(line, "add x %d", &v)
			k = append(k, v)
		case 15:
			fmt.Sscanf(line, "add y %d", &v)
			m = append(m, v)
		}
	}

	constraints := map[int][2]int{}
	var stack []int
	for i := range l {
		switch l[i] {
		case 1:
			stack = append(stack, i)
		case 26:
			pop := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			constraints[pop] = [2]int{i, m[pop] + k[i]}
		}
	}

	var max [14]int
	for i := 0; i < 14; i++ {
		if _, ok := constraints[i]; !ok {
			continue
		}
		vmax := 9
		for vmax+constraints[i][1] > 9 {
			vmax--
		}
		max[i] = vmax
		max[constraints[i][0]] = vmax + constraints[i][1]
	}

	fmt.Println(num(max))
}

func num(w [14]int) int {
	var n int
	for i := range w {
		n *= 10
		n += w[i]
	}
	return n
}

type machine struct {
	r     [4]int
	prog  string
	debug bool
}

func (m *machine) reset() {
	m.r = [4]int{0, 0, 0, 0}
}

func (m *machine) run(input string) {
	var pos int
	for _, line := range strings.Split(m.prog, "\n") {
		if m.debug {
			fmt.Println(line)
		}
		f := strings.Fields(line)
		switch f[0] {
		case "inp":
			m.r[reg(f[1])] = int(input[pos] - '0')
			pos++
		case "add":
			m.r[reg(f[1])] += m.get(f[2])
		case "mul":
			m.r[reg(f[1])] *= m.get(f[2])
		case "div":
			m.r[reg(f[1])] /= m.get(f[2])
		case "mod":
			m.r[reg(f[1])] %= m.get(f[2])
		case "eql":
			if m.r[reg(f[1])] == m.get(f[2]) {
				m.r[reg(f[1])] = 1
			} else {
				m.r[reg(f[1])] = 0
			}
		default:
			panic("unknown")
		}
		if m.debug {
			fmt.Printf("  %10v %10v %10v %10v\n", m.get("w"), m.get("x"), m.get("y"), m.get("z"))
		}
	}
}

func (m *machine) get(s string) int {
	switch s {
	case "w", "x", "y", "z":
		return m.r[reg(s)]
	default:
		return atoi(s)
	}
}

func reg(r string) int {
	return int(r[0] - 'w')
}

func manual(s string) int {
	var z int
	//     0   1   2   3   4   5   6   7   8   9  10  11  12  13
	// k  11  14  10  14  -8  14 -11  10  -6  -9  12  -5  -4  -9
	// l   1   1   1   1  26   1  26   1  26  26   1  26  26  26
	// m   7   8  16   8   3  12   1   8   8  14   4  14  15   6
	k := []int{11, 14, 10, 14, -8, 14, -11, 10, -6, -9, 12, -5, -4, -9}
	l := []int{1, 1, 1, 1, 26, 1, 26, 1, 26, 26, 1, 26, 26, 26}
	m := []int{7, 8, 16, 8, 3, 12, 1, 8, 8, 14, 4, 14, 15, 6}
	w := make([]int, 14)
	for i := 0; i < 14; i++ {
		w[i] = int(s[i] - '0')
	}
	for i := 0; i < 14; i++ {
		x := z%26 + k[i]
		if l[i] == 1 {
			z *= 26
			z += w[i] + m[i]
		} else {
			z /= 26
			if x != w[i] {
				z *= 26
				z += w[i] + m[i]
			}
		}
	}
	return z
}

func readAll(path string) string {
	file, err := os.ReadFile(path)
	if err != nil {
		panic(err)
	}
	return strings.TrimSpace(string(file))
}

func atoi(s string) int {
	n, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return n
}