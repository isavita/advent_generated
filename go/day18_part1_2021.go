package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type SnailNumber struct {
	Value int
	Left  *SnailNumber
	Right *SnailNumber
}

func (sn *SnailNumber) IsRegular() bool {
	return sn.Left == nil && sn.Right == nil
}

func (sn *SnailNumber) Add(other *SnailNumber) *SnailNumber {
	newNumber := &SnailNumber{Left: sn, Right: other}
	return newNumber.Reduce()
}

func (sn *SnailNumber) Reduce() *SnailNumber {
	for {
		exploded, _, _ := sn.Explode(0)
		if exploded {
			continue
		}
		if !sn.Split() {
			break
		}
	}
	return sn
}

func (sn *SnailNumber) Explode(depth int) (bool, int, int) {
	if sn.IsRegular() {
		return false, 0, 0
	}

	if depth == 4 {
		leftValue := sn.Left.Value
		rightValue := sn.Right.Value
		sn.Left = nil
		sn.Right = nil
		sn.Value = 0
		return true, leftValue, rightValue
	}

	exploded, leftValue, rightValue := sn.Left.Explode(depth + 1)
	if exploded {
		if rightValue > 0 && sn.Right != nil {
			sn.Right.AddLeft(rightValue)
		}
		return true, leftValue, 0
	}

	exploded, leftValue, rightValue = sn.Right.Explode(depth + 1)
	if exploded {
		if leftValue > 0 && sn.Left != nil {
			sn.Left.AddRight(leftValue)
		}
		return true, 0, rightValue
	}

	return false, 0, 0
}

func (sn *SnailNumber) AddLeft(value int) {
	if sn.IsRegular() {
		sn.Value += value
	} else {
		sn.Left.AddLeft(value)
	}
}

func (sn *SnailNumber) AddRight(value int) {
	if sn.IsRegular() {
		sn.Value += value
	} else {
		sn.Right.AddRight(value)
	}
}

func (sn *SnailNumber) Split() bool {
	if sn.IsRegular() {
		if sn.Value >= 10 {
			sn.Left = &SnailNumber{Value: sn.Value / 2}
			sn.Right = &SnailNumber{Value: (sn.Value + 1) / 2}
			sn.Value = -1
			return true
		}
		return false
	}
	return sn.Left.Split() || sn.Right.Split()
}

func (sn *SnailNumber) Magnitude() int {
	if sn.IsRegular() {
		return sn.Value
	}
	return 3*sn.Left.Magnitude() + 2*sn.Right.Magnitude()
}

func parseSnailNumber(input string) *SnailNumber {
	input = strings.TrimSpace(input)
	if input[0] != '[' {
		value, _ := strconv.Atoi(input)
		return &SnailNumber{Value: value}
	}

	balance := 0
	splitIndex := 0
	for i, char := range input[1 : len(input)-1] {
		switch char {
		case '[':
			balance++
		case ']':
			balance--
		case ',':
			if balance == 0 {
				splitIndex = i + 1 // Adjust index because we trimmed the first '['
				break
			}
		}
		if splitIndex != 0 {
			break
		}
	}

	left := parseSnailNumber(input[1:splitIndex])
	right := parseSnailNumber(input[splitIndex+1 : len(input)-1])
	return &SnailNumber{Left: left, Right: right}
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening input file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var snailNumbers []*SnailNumber
	for scanner.Scan() {
		line := scanner.Text()
		snailNumbers = append(snailNumbers, parseSnailNumber(line))
	}

	if len(snailNumbers) == 0 {
		fmt.Println("No snailfish numbers found in the file.")
		return
	}

	result := snailNumbers[0]
	for i := 1; i < len(snailNumbers); i++ {
		result = result.Add(snailNumbers[i])
	}

	fmt.Println(result.Magnitude())
}