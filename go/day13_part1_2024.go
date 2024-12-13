package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Machine struct {
	ax, ay, bx, by, px, py int
}

func main() {
	machines := readInput("input.txt")
	results := []int{}
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
	// The goal: maximize number of prizes won and minimize total cost
	// Since there's no stated limit, we just sum all minimal solutions found.
	// If we needed a more complex selection, we would do that, but the prompt implies we sum all possible.
	count := len(results)
	sum := 0
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

func parseLine(s string) (int, int) {
	parts := strings.Split(strings.TrimSpace(s), ",")
	xp := strings.TrimSpace(parts[0])
	yp := strings.TrimSpace(parts[1])
	x := parseVal(xp)
	y := parseVal(yp)
	return x, y
}

func parsePrize(s string) (int, int) {
	parts := strings.Split(strings.TrimSpace(s), ",")
	xp := strings.TrimSpace(parts[0])
	yp := strings.TrimSpace(parts[1])
	x := parseValPrize(xp)
	y := parseValPrize(yp)
	return x, y
}

func parseVal(s string) int {
	s = strings.TrimSpace(s)
	s = strings.TrimPrefix(s, "X+")
	s = strings.TrimPrefix(s, "Y+")
	s = strings.TrimPrefix(s, "X=")
	s = strings.TrimPrefix(s, "Y=")
	v, _ := strconv.Atoi(s)
	return v
}

func parseValPrize(s string) int {
	s = strings.TrimSpace(s)
	s = strings.TrimPrefix(s, "X=")
	s = strings.TrimPrefix(s, "Y=")
	v, _ := strconv.Atoi(s)
	return v
}

func solveMachine(m Machine) int {
	// We try all combinations of A and B presses up to 100
	// and see if we can reach the exact prize coords.
	minCost := -1
	for aCount := 0; aCount <= 100; aCount++ {
		for bCount := 0; bCount <= 100; bCount++ {
			x := m.ax*aCount + m.bx*bCount
			y := m.ay*aCount + m.by*bCount
			if x == m.px && y == m.py {
				cost := aCount*3 + bCount
				if minCost < 0 || cost < minCost {
					minCost = cost
				}
			}
		}
	}
	return minCost
}
