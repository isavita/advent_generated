package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

type Robot struct {
	pos       Point
	direction int
}

func (r *Robot) turn(direction int) {
	if direction == 0 {
		r.direction = (r.direction - 1 + 4) % 4
	} else {
		r.direction = (r.direction + 1) % 4
	}
}

func (r *Robot) move() {
	switch r.direction {
	case 0:
		r.pos.y++
	case 1:
		r.pos.x++
	case 2:
		r.pos.y--
	case 3:
		r.pos.x--
	}
}

type Intcode struct {
	memory  map[int]int
	pc      int
	relBase int
}

func (ic *Intcode) run(input func() int, output func(int)) {
	for {
		opcode := ic.memory[ic.pc] % 100
		modes := [3]int{
			(ic.memory[ic.pc] / 100) % 10,
			(ic.memory[ic.pc] / 1000) % 10,
			(ic.memory[ic.pc] / 10000) % 10,
		}

		getParam := func(index int) int {
			param := ic.memory[ic.pc+index+1]
			switch modes[index] {
			case 0:
				return ic.memory[param]
			case 1:
				return param
			case 2:
				return ic.memory[ic.relBase+param]
			}
			return 0
		}

		setParam := func(index, value int) {
			param := ic.memory[ic.pc+index+1]
			if modes[index] == 2 {
				param += ic.relBase
			}
			ic.memory[param] = value
		}

		switch opcode {
		case 1:
			setParam(2, getParam(0)+getParam(1))
			ic.pc += 4
		case 2:
			setParam(2, getParam(0)*getParam(1))
			ic.pc += 4
		case 3:
			setParam(0, input())
			ic.pc += 2
		case 4:
			output(getParam(0))
			ic.pc += 2
		case 5:
			if getParam(0) != 0 {
				ic.pc = getParam(1)
			} else {
				ic.pc += 3
			}
		case 6:
			if getParam(0) == 0 {
				ic.pc = getParam(1)
			} else {
				ic.pc += 3
			}
		case 7:
			if getParam(0) < getParam(1) {
				setParam(2, 1)
			} else {
				setParam(2, 0)
			}
			ic.pc += 4
		case 8:
			if getParam(0) == getParam(1) {
				setParam(2, 1)
			} else {
				setParam(2, 0)
			}
			ic.pc += 4
		case 9:
			ic.relBase += getParam(0)
			ic.pc += 2
		case 99:
			return
		}
	}
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Scan()
	input := scanner.Text()

	program := make(map[int]int)
	for i, v := range strings.Split(input, ",") {
		program[i], _ = strconv.Atoi(v)
	}

	ic := &Intcode{memory: program}
	robot := &Robot{direction: 0}
	panels := make(map[Point]int)
	outputCount := 0

	ic.run(
		func() int { return panels[robot.pos] },
		func(out int) {
			if outputCount%2 == 0 {
				panels[robot.pos] = out
			} else {
				robot.turn(out)
				robot.move()
			}
			outputCount++
		},
	)

	fmt.Println(len(panels))
}
