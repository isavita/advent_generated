package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

type state struct {
	x, y, d int
}

func main() {
	f, _ := os.Open("input.txt")
	defer f.Close()

	var grid []string
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}

	n, m := len(grid), len(grid[0])
	var sx, sy, ex, ey int
	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			if grid[i][j] == 'S' {
				sx, sy = i, j
			} else if grid[i][j] == 'E' {
				ex, ey = i, j
			}
		}
	}

	// Directions: 0:N,1:E,2:S,3:W
	dx := []int{-1,0,1,0}
	dy := []int{0,1,0,-1}

	dist := make([][][]int, n)
	for i := range dist {
		dist[i] = make([][]int, m)
		for j := range dist[i] {
			dist[i][j] = []int{math.MaxInt, math.MaxInt, math.MaxInt, math.MaxInt}
		}
	}
	dist[sx][sy][1] = 0

	h := &minHeap{}
	h.push(node{sx, sy, 1, 0})

	for len(h.a) > 0 {
		u := h.pop()
		if dist[u.x][u.y][u.d] < u.cost {
			continue
		}
		if u.x == ex && u.y == ey {
			fmt.Println(u.cost)
			return
		}
		for _, ndir := range []int{(u.d+1)%4, (u.d+3)%4} {
			nc := u.cost + 1000
			if nc < dist[u.x][u.y][ndir] {
				dist[u.x][u.y][ndir] = nc
				h.push(node{u.x, u.y, ndir, nc})
			}
		}
		nx, ny := u.x+dx[u.d], u.y+dy[u.d]
		if nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] != '#' {
			nc := u.cost + 1
			if nc < dist[nx][ny][u.d] {
				dist[nx][ny][u.d] = nc
				h.push(node{nx, ny, u.d, nc})
			}
		}
	}
}

type node struct {
	x, y, d, cost int
}

type minHeap struct{ a []node }

func (h *minHeap) push(v node) {
	h.a = append(h.a, v)
	h.up(len(h.a)-1)
}

func (h *minHeap) pop() node {
	v := h.a[0]
	h.a[0] = h.a[len(h.a)-1]
	h.a = h.a[:len(h.a)-1]
	h.down(0)
	return v
}

func (h *minHeap) up(i int) {
	for i > 0 {
		p := (i-1)>>1
		if h.a[p].cost <= h.a[i].cost {
			break
		}
		h.a[p], h.a[i] = h.a[i], h.a[p]
		i = p
	}
}

func (h *minHeap) down(i int) {
	for {
		l := 2*i+1
		r := 2*i+2
		small := i
		if l < len(h.a) && h.a[l].cost < h.a[small].cost {
			small = l
		}
		if r < len(h.a) && h.a[r].cost < h.a[small].cost {
			small = r
		}
		if small == i {
			break
		}
		h.a[i], h.a[small] = h.a[small], h.a[i]
		i = small
	}
}
