package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"math"
	"strconv"
	"strings"
)

type VM struct {
	Code         map[int]int // source code and memory
	Ip           int         // Instruction Pointer
	Input        chan int
	Output       chan int
	RelativeBase int
}

func NewVM(filename string) *VM {
	input := make(chan int, 0)
	output := make(chan int, 0)

	vm := &VM{
		Code:   nil,
		Ip:     0,
		Input:  input,
		Output: output,
	}

	vm.Load(filename)

	return vm
}

func (vm *VM) Load(filename string) {
	buf, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Fatal(err)
	}
	listStr := strings.Split(strings.TrimSpace(string(buf)), ",")
	code := make(map[int]int, len(listStr))

	for i := range listStr {
		var err error
		code[i], err = strconv.Atoi(listStr[i])
		if err != nil {
			log.Fatal(err)
		}
	}
	vm.Code = code
	vm.Ip = 0
	vm.RelativeBase = 0
}

func (vm *VM) Run() {
	var arity int

	for {
		cmd := Cmd(vm.Code[vm.Ip])

		switch cmd.OpCode() {

		case 1:
			// add
			arity = 3
			params := vm.getParamsAddresses(vm.Ip, cmd, arity)
			vm.Code[params[2]] = vm.Code[params[0]] + vm.Code[params[1]]

		case 2:
			// multiply
			arity = 3
			params := vm.getParamsAddresses(vm.Ip, cmd, arity)
			vm.Code[params[2]] = vm.Code[params[0]] * vm.Code[params[1]]

		case 3:
			// read
			arity = 1
			params := vm.getParamsAddresses(vm.Ip, cmd, arity)
			vm.Code[params[0]] = <-vm.Input

		case 4:
			// write
			arity = 1
			params := vm.getParamsAddresses(vm.Ip, cmd, arity)
			vm.Output <- vm.Code[params[0]]

		case 5:
			// jump not zero
			arity = 2
			params := vm.getParamsAddresses(vm.Ip, cmd, arity)
			if vm.Code[params[0]] != 0 {
				vm.Ip = vm.Code[params[1]]
				continue
			}

		case 6:
			// jump zero
			arity = 2
			params := vm.getParamsAddresses(vm.Ip, cmd, arity)
			if vm.Code[params[0]] == 0 {
				vm.Ip = vm.Code[params[1]]
				continue
			}

		case 7:
			// less than
			arity = 3
			params := vm.getParamsAddresses(vm.Ip, cmd, arity)

			if vm.Code[params[0]] < vm.Code[params[1]] {
				vm.Code[params[2]] = 1
			} else {
				vm.Code[params[2]] = 0
			}

		case 8:
			// equal
			arity = 3
			params := vm.getParamsAddresses(vm.Ip, cmd, arity)

			if vm.Code[params[0]] == vm.Code[params[1]] {
				vm.Code[params[2]] = 1
			} else {
				vm.Code[params[2]] = 0
			}
		case 9:
			// change relative base
			arity = 1
			params := vm.getParamsAddresses(vm.Ip, cmd, arity)
			vm.RelativeBase += vm.Code[params[0]]

		case 99:
			// halt
			return

		default:
			log.Fatalf("not an opcode %v", cmd)
		}

		vm.Ip += arity + 1
	}
}

func (vm *VM) getParamsAddresses(pos int, cmd Cmd, arity int) []int {
	modes := cmd.Modes(arity)
	results := make([]int, arity)
	for i := 0; i < arity; i++ {
		results[i] = vm.getParamAddress(pos+i+1, modes[i])
	}
	return results
}

func (vm *VM) getParamAddress(pos int, mode Mode) int {
	switch mode {
	case Position:
		return vm.Code[pos]
	case Immediate:
		return pos
	case Relative:
		return vm.RelativeBase + vm.Code[pos]
	}
	log.Fatal("wrong mode")
	return -1
}

type Mode int

const (
	Position Mode = iota
	Immediate
	Relative
)

type Cmd int

func (c Cmd) OpCode() int {
	// last 2 digits
	return int(c) % 100
}

func (c Cmd) Modes(arity int) []Mode {
	// remove last two digits
	modeSection := int(c) / 100

	modes := make([]Mode, arity)
	for i := 0; i < arity; i++ {
		modes[i] = Mode(modeSection / int(math.Pow10(i)) % 10)
	}
	return modes
}

func main() {
	sum := 0
	for y := 0; y < 50; y++ {
		for x := 0; x < 50; x++ {
			if Beam(x, y) {
				sum++
			}
		}
	}

	fmt.Println(sum)
}

func Beam(x, y int) bool {
	vm := NewVM("input.txt")

	go vm.Run()

	vm.Input <- x
	vm.Input <- y

	beam := <-vm.Output

	return beam == 1
}