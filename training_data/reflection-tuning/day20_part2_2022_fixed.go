package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

type Number struct {
	value int64
	originalIndex int
}

func mix(numbers []Number, times int) {
	length := int64(len(numbers))
	for t := 0; t < times; t++ {
		for i := 0; i < len(numbers); i++ {
			for j, num := range numbers {
				if num.originalIndex == i {
					newPos := (int64(j) + num.value) % (length - 1)
					if newPos <= 0 {
						newPos += length - 1
					}
					numbers = append(numbers[:j], numbers[j+1:]...)
					numbers = append(numbers[:newPos], append([]Number{num}, numbers[newPos:]...)...)
					break
				}
			}
		}
	}
}

func findGroveCoordinates(numbers []Number) int64 {
	zeroIndex := 0
	for i, num := range numbers {
		if num.value == 0 {
			zeroIndex = i
			break
		}
	}
	length := len(numbers)
	sum := numbers[(zeroIndex+1000)%length].value +
		   numbers[(zeroIndex+2000)%length].value +
		   numbers[(zeroIndex+3000)%length].value
	return sum
}

func main() {
	input, _ := ioutil.ReadFile("input.txt")
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	numbers := make([]Number, len(lines))
	for i, line := range lines {
		value, _ := strconv.ParseInt(line, 10, 64)
		numbers[i] = Number{value: value, originalIndex: i}
	}

	// Part 1
	numbersCopy := make([]Number, len(numbers))
	copy(numbersCopy, numbers)
	mix(numbersCopy, 1)
	fmt.Println("Part 1:", findGroveCoordinates(numbersCopy))

	// Part 2
	decryptionKey := int64(811589153)
	for i := range numbers {
		numbers[i].value *= decryptionKey
	}
	mix(numbers, 10)
	fmt.Println("Part 2:", findGroveCoordinates(numbers))
}
