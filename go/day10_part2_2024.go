package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

type pos struct{ r, c int }

var dirs = []pos{{1,0},{-1,0},{0,1},{0,-1}}

func main() {
	data, _ := ioutil.ReadFile("input.txt")
	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	nr := len(lines)
	nc := len(lines[0])
	grid := make([][]int, nr)
	for i := 0; i < nr; i++ {
		grid[i] = make([]int, nc)
		for j := 0; j < nc; j++ {
			grid[i][j] = int(lines[i][j]-'0')
		}
	}

	dp := make([][]int64, nr)
	for i := 0; i < nr; i++ {
		dp[i] = make([]int64, nc)
		for j := 0; j < nc; j++ {
			dp[i][j] = -1
		}
	}

	var dfs func(r,c int) int64
	dfs = func(r,c int) int64 {
		if dp[r][c] != -1 {
			return dp[r][c]
		}
		h := grid[r][c]
		if h == 9 {
			dp[r][c] = 1
			return 1
		}
		var sum int64
		for _, d := range dirs {
			nr2,nc2 := r+d.r,c+d.c
			if nr2<0||nr2>=nr||nc2<0||nc2>=nc { continue }
			if grid[nr2][nc2] == h+1 {
				sum += dfs(nr2,nc2)
			}
		}
		dp[r][c] = sum
		return sum
	}

	var total int64
	for r := 0; r < nr; r++ {
		for c := 0; c < nc; c++ {
			if grid[r][c] == 0 {
				total += dfs(r,c)
			}
		}
	}
	fmt.Println(total)
}
