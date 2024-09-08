package main

import (
	"fmt"
	"math/bits"
	"strconv"
	"strings"
)

func main() {
	input := "flqrgnkx" // Replace with your actual input
	usedSquares := countUsedSquares(input)
	fmt.Printf("Number of used squares: %d\n", usedSquares)
}

func countUsedSquares(key string) int {
	count := 0
	for i := 0; i < 128; i++ {
		hash := KnotHash(fmt.Sprintf("%s-%d", key, i))
		count += countBits(hash)
	}
	return count
}

func countBits(hash string) int {
	count := 0
	for _, ch := range hash {
		n, _ := strconv.ParseUint(string(ch), 16, 8)
		count += bits.OnesCount8(uint8(n))
	}
	return count
}

func KnotHash(input string) string {
	lengths := make([]int, 0, len(input))
	for _, ch := range input {
		lengths = append(lengths, int(ch))
	}
	lengths = append(lengths, 17, 31, 73, 47, 23)

	list := make([]int, 256)
	for i := range list {
		list[i] = i
	}

	pos, skip := 0, 0
	for round := 0; round < 64; round++ {
		for _, length := range lengths {
			reverse(list, pos, length)
			pos = (pos + length + skip) % len(list)
			skip++
		}
	}

	dense := make([]int, 16)
	for i := 0; i < 16; i++ {
		for j := 0; j < 16; j++ {
			dense[i] ^= list[i*16+j]
		}
	}

	var hash strings.Builder
	for _, n := range dense {
		hash.WriteString(fmt.Sprintf("%02x", n))
	}
	return hash.String()
}

func reverse(list []int, start, length int) {
	for i := 0; i < length/2; i++ {
		a, b := (start+i)%len(list), (start+length-1-i)%len(list)
		list[a], list[b] = list[b], list[a]
	}
}
