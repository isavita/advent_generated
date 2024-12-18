package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

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
	for i := 0; i < 1024 && scanner.Scan(); i++ {
		line := scanner.Text()
		parts := strings.Split(line, ",")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		if x >= 0 && x < size && y >= 0 && y < size {
			grid[y][x] = true
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	dirs := [][]int{{1,0},{-1,0},{0,1},{0,-1}}
	visited := make([][]bool, size)
	for i := range visited {
		visited[i] = make([]bool, size)
	}

	type pt struct{ x,y int }
	q := []struct{pt;steps int}{{pt{0,0},0}}
	visited[0][0] = true

	for len(q) > 0 {
		cur := q[0]
		q = q[1:]
		if cur.x == size-1 && cur.y == size-1 {
			fmt.Println(cur.steps)
			return
		}
		for _, d := range dirs {
			nx, ny := cur.x+d[0], cur.y+d[1]
			if nx>=0 && ny>=0 && nx<size && ny<size && !grid[ny][nx] && !visited[ny][nx] {
				visited[ny][nx]=true
				q=append(q, struct{pt;steps int}{pt{nx,ny},cur.steps+1})
			}
		}
	}
	fmt.Println("No path")
}
