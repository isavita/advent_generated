package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type SnailfishNumber struct {
	value        int
	left, right  *SnailfishNumber
	parent       *SnailfishNumber
	isRegular    bool
}

func parseSnailfishNumber(s string) *SnailfishNumber {
	if s[0] != '[' {
		value, _ := strconv.Atoi(s)
		return &SnailfishNumber{value: value, isRegular: true}
	}

	root := &SnailfishNumber{}
	stack := []*SnailfishNumber{root}
	current := root

	for i := 1; i < len(s)-1; i++ {
		switch s[i] {
		case '[':
			newPair := &SnailfishNumber{parent: current}
			if current.left == nil {
				current.left = newPair
			} else {
				current.right = newPair
			}
			stack = append(stack, newPair)
			current = newPair
		case ']':
			stack = stack[:len(stack)-1]
			if len(stack) > 0 {
				current = stack[len(stack)-1]
			}
		case ',':
			// Do nothing
		default:
			value, _ := strconv.Atoi(string(s[i]))
			newNumber := &SnailfishNumber{value: value, isRegular: true, parent: current}
			if current.left == nil {
				current.left = newNumber
			} else {
				current.right = newNumber
			}
		}
	}

	return root
}

func (sn *SnailfishNumber) reduce() {
	for {
		if sn.explode(0) {
			continue
		}
		if sn.split() {
			continue
		}
		break
	}
}

func (sn *SnailfishNumber) explode(depth int) bool {
	if sn.isRegular {
		return false
	}

	if depth == 4 {
		left := sn.findRegularLeft()
		if left != nil {
			left.value += sn.left.value
		}

		right := sn.findRegularRight()
		if right != nil {
			right.value += sn.right.value
		}

		sn.value = 0
		sn.isRegular = true
		sn.left = nil
		sn.right = nil
		return true
	}

	return sn.left.explode(depth+1) || sn.right.explode(depth+1)
}

func (sn *SnailfishNumber) split() bool {
	if sn.isRegular {
		if sn.value >= 10 {
			sn.left = &SnailfishNumber{value: sn.value / 2, isRegular: true, parent: sn}
			sn.right = &SnailfishNumber{value: (sn.value + 1) / 2, isRegular: true, parent: sn}
			sn.isRegular = false
			sn.value = 0
			return true
		}
		return false
	}

	return sn.left.split() || sn.right.split()
}

func (sn *SnailfishNumber) findRegularLeft() *SnailfishNumber {
	current := sn
	for current.parent != nil {
		if current == current.parent.right {
			current = current.parent.left
			for !current.isRegular {
				current = current.right
			}
			return current
		}
		current = current.parent
	}
	return nil
}

func (sn *SnailfishNumber) findRegularRight() *SnailfishNumber {
	current := sn
	for current.parent != nil {
		if current == current.parent.left {
			current = current.parent.right
			for !current.isRegular {
				current = current.left
			}
			return current
		}
		current = current.parent
	}
	return nil
}

func add(a, b *SnailfishNumber) *SnailfishNumber {
	result := &SnailfishNumber{left: a, right: b}
	a.parent = result
	b.parent = result
	result.reduce()
	return result
}

func (sn *SnailfishNumber) magnitude() int {
	if sn.isRegular {
		return sn.value
	}
	return 3*sn.left.magnitude() + 2*sn.right.magnitude()
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	var numbers []*SnailfishNumber

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		numbers = append(numbers, parseSnailfishNumber(line))
	}

	result := numbers[0]
	for i := 1; i < len(numbers); i++ {
		result = add(result, numbers[i])
	}

	fmt.Println(result.magnitude())
}
