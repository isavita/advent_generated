package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

type pos struct{ r, c int }

func main() {
	data, _ := ioutil.ReadFile("input.txt")
	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	nr := len(lines)
	nc := len(lines[0])
	grid := make([][]int, nr)
	for i := 0; i < nr; i++ {
		grid[i] = make([]int, nc)
		for j := 0; j < nc; j++ {
			grid[i][j] = int(lines[i][j] - '0')
		}
	}

	dirs := []pos{{1,0},{-1,0},{0,1},{0,-1}}
	var trailheads []pos
	for r := 0; r < nr; r++ {
		for c := 0; c < nc; c++ {
			if grid[r][c] == 0 {
				trailheads = append(trailheads, pos{r,c})
			}
		}
	}

	sumScores := 0
	for _, th := range trailheads {
		reached := make(map[pos]bool)
		front := []struct {
			p pos
			h int
		}{{th,0}}
		visited := make(map[[3]int]bool)
		for len(front) > 0 {
			cur := front[len(front)-1]
			front = front[:len(front)-1]
			if cur.h == 9 {
				if !reached[cur.p] {
					reached[cur.p] = true
				}
				continue
			}
			for _, d := range dirs {
				nr2, nc2 := cur.p.r+d.r, cur.p.c+d.c
				if nr2<0||nr2>=nr||nc2<0||nc2>=nc { continue }
				if grid[nr2][nc2] == cur.h+1 {
					key := [3]int{nr2,nc2,cur.h+1}
					if !visited[key] {
						visited[key] = true
						front = append(front, struct{p pos;h int}{pos{nr2,nc2},cur.h+1})
					}
				}
			}
		}
		sumScores += len(reached)
	}

	fmt.Println(sumScores)
}
