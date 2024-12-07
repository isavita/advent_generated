package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func concat(a, b int) int {
	sa := strconv.Itoa(a)
	sb := strconv.Itoa(b)
	v, _ := strconv.Atoi(sa + sb)
	return v
}

func canProduce(target int, nums []int, idx int, value int) bool {
	if idx == len(nums) {
		return value == target
	}
	n := nums[idx]
	if canProduce(target, nums, idx+1, value+n) {
		return true
	}
	if canProduce(target, nums, idx+1, value*n) {
		return true
	}
	if canProduce(target, nums, idx+1, concat(value, n)) {
		return true
	}
	return false
}

func main() {
	f, _ := os.Open("input.txt")
	defer f.Close()
	sc := bufio.NewScanner(f)
	total := 0
	for sc.Scan() {
		line := sc.Text()
		if line == "" {
			continue
		}
		parts := strings.Split(line, ":")
		target, _ := strconv.Atoi(strings.TrimSpace(parts[0]))
		numStrs := strings.Fields(strings.TrimSpace(parts[1]))
		if len(numStrs) == 0 {
			continue
		}
		nums := make([]int, len(numStrs))
		for i, ns := range numStrs {
			nums[i], _ = strconv.Atoi(ns)
		}

		if len(nums) == 1 {
			if nums[0] == target {
				total += target
			}
			continue
		}

		if canProduce(target, nums, 1, nums[0]) {
			total += target
		}
	}
	fmt.Println(total)
}
