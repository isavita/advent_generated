package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

const listSize = 256

func reverse(list []int, start, length int) {
	for i := 0; i < length/2; i++ {
		a, b := (start+i)%listSize, (start+length-1-i)%listSize
		list[a], list[b] = list[b], list[a]
	}
}

func knotTie(list []int, lengths []int, rounds int) []int {
	position, skip := 0, 0
	for round := 0; round < rounds; round++ {
		for _, length := range lengths {
			reverse(list, position, length)
			position = (position + length + skip) % listSize
			skip++
		}
	}
	return list
}

func partOne(input string) int {
	lengths := []int{}
	for _, s := range strings.Split(input, ",") {
		n, _ := strconv.Atoi(s)
		lengths = append(lengths, n)
	}

	list := make([]int, listSize)
	for i := range list {
		list[i] = i
	}

	result := knotTie(list, lengths, 1)
	return result[0] * result[1]
}

func partTwo(input string) string {
	lengths := []int{}
	for _, ch := range input {
		lengths = append(lengths, int(ch))
	}
	lengths = append(lengths, 17, 31, 73, 47, 23)

	list := make([]int, listSize)
	for i := range list {
		list[i] = i
	}

	sparseHash := knotTie(list, lengths, 64)

	denseHash := make([]byte, 16)
	for i := 0; i < 16; i++ {
		xor := 0
		for j := 0; j < 16; j++ {
			xor ^= sparseHash[i*16+j]
		}
		denseHash[i] = byte(xor)
	}

	return fmt.Sprintf("%x", denseHash)
}

func main() {
	input, _ := ioutil.ReadFile("input.txt")
	trimmedInput := strings.TrimSpace(string(input))

	fmt.Println("Part One:", partOne(trimmedInput))
	fmt.Println("Part Two:", partTwo(trimmedInput))
}
