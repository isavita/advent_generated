package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func canReach(grid [][]bool) bool {
	n := len(grid)
	if grid[0][0] || grid[n-1][n-1] {
		return false
	}
	dirs := [][]int{{1,0},{-1,0},{0,1},{0,-1}}
	visited := make([][]bool, n)
	for i := range visited {
		visited[i] = make([]bool, n)
	}
	type pt struct{ x,y int }
	q := []pt{{0,0}}
	visited[0][0] = true
	for len(q)>0 {
		c := q[0]
		q = q[1:]
		if c.x==n-1 && c.y==n-1 {
			return true
		}
		for _,d:=range dirs {
			nx,ny:=c.x+d[0],c.y+d[1]
			if nx>=0 && ny>=0 && nx<n && ny<n && !grid[ny][nx] && !visited[ny][nx]{
				visited[ny][nx]=true
				q=append(q, pt{nx,ny})
			}
		}
	}
	return false
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	const size = 71
	grid := make([][]bool, size)
	for i := range grid {
		grid[i] = make([]bool, size)
	}
	scanner := bufio.NewScanner(f)
	i := 0
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, ",")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		if x>=0&&x<size && y>=0&&y<size {
			grid[y][x]=true
		}
		i++
		if !canReach(grid) {
			fmt.Printf("%d,%d\n", x, y)
			return
		}
	}
	fmt.Println("No cutoff found")
}
