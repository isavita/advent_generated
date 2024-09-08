package main

import (
	"fmt"
	"strconv"
	"strings"
)

const gridSize = 128

func knotHash(input string) string {
	lengths := make([]int, 0, len(input))
	for _, c := range input {
		lengths = append(lengths, int(c))
	}
	lengths = append(lengths, 17, 31, 73, 47, 23)

	list := make([]int, 256)
	for i := range list {
		list[i] = i
	}

	var pos, skip int
	for round := 0; round < 64; round++ {
		for _, length := range lengths {
			for i := 0; i < length/2; i++ {
				a, b := (pos+i)%256, (pos+length-1-i)%256
				list[a], list[b] = list[b], list[a]
			}
			pos = (pos + length + skip) % 256
			skip++
		}
	}

	dense := make([]byte, 16)
	for i := 0; i < 16; i++ {
		for j := 0; j < 16; j++ {
			dense[i] ^= byte(list[i*16+j])
		}
	}

	return fmt.Sprintf("%x", dense)
}

func hexToBin(hex string) string {
	bin := ""
	for _, c := range hex {
		n, _ := strconv.ParseUint(string(c), 16, 8)
		bin += fmt.Sprintf("%04b", n)
	}
	return bin
}

func countUsedSquares(key string) int {
	count := 0
	for i := 0; i < gridSize; i++ {
		hash := knotHash(fmt.Sprintf("%s-%d", key, i))
		bin := hexToBin(hash)
		count += strings.Count(bin, "1")
	}
	return count
}

func countRegions(key string) int {
	grid := make([][]bool, gridSize)
	for i := range grid {
		grid[i] = make([]bool, gridSize)
		hash := knotHash(fmt.Sprintf("%s-%d", key, i))
		bin := hexToBin(hash)
		for j, bit := range bin {
			if bit == '1' {
				grid[i][j] = true
			}
		}
	}

	regions := 0
	for i := range grid {
		for j := range grid[i] {
			if grid[i][j] {
				regions++
				dfs(grid, i, j)
			}
		}
	}
	return regions
}

func dfs(grid [][]bool, i, j int) {
	if i < 0 || i >= gridSize || j < 0 || j >= gridSize || !grid[i][j] {
		return
	}
	grid[i][j] = false
	dfs(grid, i+1, j)
	dfs(grid, i-1, j)
	dfs(grid, i, j+1)
	dfs(grid, i, j-1)
}

func main() {
	key := "flqrgnkx" // Replace with your actual key
	usedSquares := countUsedSquares(key)
	regions := countRegions(key)
	fmt.Printf("Part 1: %d squares are used\n", usedSquares)
	fmt.Printf("Part 2: %d regions are present\n", regions)
}
