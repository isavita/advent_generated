package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	input, _ := ioutil.ReadFile("input.txt")
	inputStr := strings.TrimSpace(string(input))
	inputNum, _ := strconv.Atoi(inputStr)

	fmt.Println("Part 1:", part1(inputNum))
	fmt.Println("Part 2:", part2(inputStr))
}

func part1(input int) string {
	recipes := []int{3, 7}
	elf1, elf2 := 0, 1

	for len(recipes) < input+10 {
		sum := recipes[elf1] + recipes[elf2]
		if sum >= 10 {
			recipes = append(recipes, sum/10, sum%10)
		} else {
			recipes = append(recipes, sum)
		}
		elf1 = (elf1 + 1 + recipes[elf1]) % len(recipes)
		elf2 = (elf2 + 1 + recipes[elf2]) % len(recipes)
	}

	result := ""
	for i := input; i < input+10; i++ {
		result += strconv.Itoa(recipes[i])
	}
	return result
}

func part2(input string) int {
	recipes := []int{3, 7}
	elf1, elf2 := 0, 1
	target := make([]int, len(input))
	for i, ch := range input {
		target[i] = int(ch - '0')
	}

	for {
		sum := recipes[elf1] + recipes[elf2]
		if sum >= 10 {
			recipes = append(recipes, sum/10, sum%10)
		} else {
			recipes = append(recipes, sum)
		}
		elf1 = (elf1 + 1 + recipes[elf1]) % len(recipes)
		elf2 = (elf2 + 1 + recipes[elf2]) % len(recipes)

		if len(recipes) >= len(target) {
			if match(recipes[len(recipes)-len(target):], target) {
				return len(recipes) - len(target)
			}
			if len(recipes) > len(target) && match(recipes[len(recipes)-len(target)-1:len(recipes)-1], target) {
				return len(recipes) - len(target) - 1
			}
		}
	}
}

func match(a, b []int) bool {
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
