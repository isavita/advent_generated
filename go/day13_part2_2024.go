package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Machine struct {
	ax, ay, bx, by, px, py int64
}

func main() {
	const offset = 10000000000000
	machines := readInput("input.txt")
	for i := range machines {
		machines[i].px += offset
		machines[i].py += offset
	}

	var results []int64
	for _, m := range machines {
		cost := solveMachine(m)
		if cost >= 0 {
			results = append(results, cost)
		}
	}
	if len(results) == 0 {
		fmt.Println("0 0")
		return
	}
	count := len(results)
	var sum int64
	for _, c := range results {
		sum += c
	}
	fmt.Printf("%d %d\n", count, sum)
}

func readInput(filename string) []Machine {
	f, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer f.Close()

	var machines []Machine
	var lines []string
	sc := bufio.NewScanner(f)
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			if len(lines) > 0 {
				m := parseMachine(lines)
				machines = append(machines, m)
				lines = []string{}
			}
		} else {
			lines = append(lines, line)
		}
	}
	if len(lines) > 0 {
		m := parseMachine(lines)
		machines = append(machines, m)
	}
	return machines
}

func parseMachine(lines []string) Machine {
	var m Machine
	for _, l := range lines {
		l = strings.ReplaceAll(l, "Button A:", "A:")
		l = strings.ReplaceAll(l, "Button B:", "B:")
		l = strings.ReplaceAll(l, "Prize:", "P:")
		if strings.HasPrefix(l, "A:") {
			m.ax, m.ay = parseLine(l[2:])
		} else if strings.HasPrefix(l, "B:") {
			m.bx, m.by = parseLine(l[2:])
		} else if strings.HasPrefix(l, "P:") {
			m.px, m.py = parsePrize(l[2:])
		}
	}
	return m
}

func parseLine(s string) (int64, int64) {
	parts := strings.Split(strings.TrimSpace(s), ",")
	x := parseVal(parts[0])
	y := parseVal(parts[1])
	return x, y
}

func parsePrize(s string) (int64, int64) {
	parts := strings.Split(strings.TrimSpace(s), ",")
	x := parseValPrize(parts[0])
	y := parseValPrize(parts[1])
	return x, y
}

func parseVal(s string) int64 {
	s = strings.TrimSpace(s)
	s = strings.TrimPrefix(s, "X+")
	s = strings.TrimPrefix(s, "Y+")
	s = strings.TrimPrefix(s, "X=")
	s = strings.TrimPrefix(s, "Y=")
	v, _ := strconv.ParseInt(s, 10, 64)
	return v
}

func parseValPrize(s string) int64 {
	s = strings.TrimSpace(s)
	s = strings.TrimPrefix(s, "X=")
	s = strings.TrimPrefix(s, "Y=")
	v, _ := strconv.ParseInt(s, 10, 64)
	return v
}

func solveMachine(m Machine) int64 {
	D := m.ax*m.by - m.ay*m.bx
	if D == 0 {
		return -1
	}
	numA := m.px*m.by - m.py*m.bx
	numB := -m.px*m.ay + m.py*m.ax
	if numA%D != 0 || numB%D != 0 {
		return -1
	}
	a := numA / D
	b := numB / D
	if a < 0 || b < 0 {
		return -1
	}
	return 3*a + b
}
