package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func canProduce(target int, nums []int, idx int, current int) bool {
	if idx == len(nums) {
		return current == target
	}
	plus := canProduce(target, nums, idx+1, current+nums[idx])
	mul := canProduce(target, nums, idx+1, current*nums[idx])
	return plus || mul
}

func main() {
	f, _ := os.Open("input.txt")
	defer f.Close()
	sc := bufio.NewScanner(f)
	var total int
	for sc.Scan() {
		line := sc.Text()
		if line == "" {
			continue
		}
		parts := strings.Split(line, ":")
		target, _ := strconv.Atoi(strings.TrimSpace(parts[0]))
		numStrs := strings.Fields(strings.TrimSpace(parts[1]))
		var nums []int
		for _, ns := range numStrs {
			v, _ := strconv.Atoi(ns)
			nums = append(nums, v)
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
