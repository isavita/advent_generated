package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type SnailfishNumber struct {
	left, right *SnailfishNumber
	value       int
	isRegular   bool
}

func parse(s string) *SnailfishNumber {
	if s[0] != '[' {
		val, _ := strconv.Atoi(s)
		return &SnailfishNumber{value: val, isRegular: true}
	}
	depth := 0
	for i, c := range s[1 : len(s)-1] {
		if c == '[' {
			depth++
		} else if c == ']' {
			depth--
		} else if c == ',' && depth == 0 {
			return &SnailfishNumber{
				left:  parse(s[1 : i+1]),
				right: parse(s[i+2 : len(s)-1]),
			}
		}
	}
	return nil
}

func (sn *SnailfishNumber) add(other *SnailfishNumber) *SnailfishNumber {
	result := &SnailfishNumber{left: sn, right: other}
	result.reduce()
	return result
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
		leftVal, rightVal := sn.left.value, sn.right.value
		sn.left, sn.right = nil, nil
		sn.value, sn.isRegular = 0, true
		sn.addLeft(leftVal)
		sn.addRight(rightVal)
		return true
	}
	return sn.left.explode(depth+1) || sn.right.explode(depth+1)
}

func (sn *SnailfishNumber) addLeft(val int) {
	if sn.isRegular {
		sn.value += val
	} else if sn.left != nil {
		sn.left.addLeft(val)
	}
}

func (sn *SnailfishNumber) addRight(val int) {
	if sn.isRegular {
		sn.value += val
	} else if sn.right != nil {
		sn.right.addRight(val)
	}
}

func (sn *SnailfishNumber) split() bool {
	if sn.isRegular {
		if sn.value >= 10 {
			sn.left = &SnailfishNumber{value: sn.value / 2, isRegular: true}
			sn.right = &SnailfishNumber{value: (sn.value + 1) / 2, isRegular: true}
			sn.isRegular = false
			return true
		}
		return false
	}
	return sn.left.split() || sn.right.split()
}

func (sn *SnailfishNumber) magnitude() int {
	if sn.isRegular {
		return sn.value
	}
	return 3*sn.left.magnitude() + 2*sn.right.magnitude()
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)

	var numbers []*SnailfishNumber
	for scanner.Scan() {
		numbers = append(numbers, parse(strings.TrimSpace(scanner.Text())))
	}

	result := numbers[0]
	for _, num := range numbers[1:] {
		result = result.add(num)
	}

	fmt.Println(result.magnitude())
}
