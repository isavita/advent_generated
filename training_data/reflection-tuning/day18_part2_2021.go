package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
)

type SnailfishNumber struct {
	value        int
	left, right  *SnailfishNumber
	parent       *SnailfishNumber
	isRegular    bool
}

func parseSnailfishNumber(s string) *SnailfishNumber {
	var parse func(s string, start int) (*SnailfishNumber, int)
	parse = func(s string, start int) (*SnailfishNumber, int) {
		if s[start] != '[' {
			end := start
			for end < len(s) && s[end] >= '0' && s[end] <= '9' {
				end++
			}
			value, _ := strconv.Atoi(s[start:end])
			return &SnailfishNumber{value: value, isRegular: true}, end
		}

		node := &SnailfishNumber{}
		i := start + 1
		node.left, i = parse(s, i)
		node.left.parent = node
		i++ // skip comma
		node.right, i = parse(s, i)
		node.right.parent = node
		return node, i + 1 // skip closing bracket
	}

	result, _ := parse(s, 0)
	return result
}

func (sn *SnailfishNumber) add(other *SnailfishNumber) *SnailfishNumber {
	result := &SnailfishNumber{left: sn.clone(), right: other.clone()}
	result.left.parent = result
	result.right.parent = result
	result.reduce()
	return result
}

func (sn *SnailfishNumber) clone() *SnailfishNumber {
	if sn.isRegular {
		return &SnailfishNumber{value: sn.value, isRegular: true}
	}
	clone := &SnailfishNumber{
		left:  sn.left.clone(),
		right: sn.right.clone(),
	}
	clone.left.parent = clone
	clone.right.parent = clone
	return clone
}

func (sn *SnailfishNumber) reduce() {
	for {
		if sn.tryExplode(0) {
			continue
		}
		if sn.trySplit() {
			continue
		}
		break
	}
}

func (sn *SnailfishNumber) tryExplode(depth int) bool {
	if sn.isRegular {
		return false
	}
	if depth == 4 {
		sn.explode()
		return true
	}
	return sn.left.tryExplode(depth+1) || sn.right.tryExplode(depth+1)
}

func (sn *SnailfishNumber) explode() {
	left := sn.findLeftNeighbor()
	if left != nil {
		left.value += sn.left.value
	}
	right := sn.findRightNeighbor()
	if right != nil {
		right.value += sn.right.value
	}
	sn.left = nil
	sn.right = nil
	sn.value = 0
	sn.isRegular = true
}

func (sn *SnailfishNumber) findLeftNeighbor() *SnailfishNumber {
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

func (sn *SnailfishNumber) findRightNeighbor() *SnailfishNumber {
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

func (sn *SnailfishNumber) trySplit() bool {
	if sn.isRegular {
		if sn.value >= 10 {
			sn.split()
			return true
		}
		return false
	}
	return sn.left.trySplit() || sn.right.trySplit()
}

func (sn *SnailfishNumber) split() {
	left := sn.value / 2
	right := (sn.value + 1) / 2
	sn.left = &SnailfishNumber{value: left, isRegular: true, parent: sn}
	sn.right = &SnailfishNumber{value: right, isRegular: true, parent: sn}
	sn.isRegular = false
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
		numbers = append(numbers, parseSnailfishNumber(scanner.Text()))
	}

	// Part 1
	result := numbers[0]
	for i := 1; i < len(numbers); i++ {
		result = result.add(numbers[i])
	}
	fmt.Println("Part 1:", result.magnitude())

	// Part 2
	maxMagnitude := 0
	for i := 0; i < len(numbers); i++ {
		for j := 0; j < len(numbers); j++ {
			if i != j {
				magnitude := numbers[i].add(numbers[j]).magnitude()
				maxMagnitude = int(math.Max(float64(maxMagnitude), float64(magnitude)))
			}
		}
	}
	fmt.Println("Part 2:", maxMagnitude)
}
