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
			// Found shortest cost, break
			// Not necessarily done because we need full dist array
			continue
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

	best := math.MaxInt
	for d := 0; d < 4; d++ {
		if dist[ex][ey][d] < best {
			best = dist[ex][ey][d]
		}
	}

	used := make([][]bool, n)
	for i := range used {
		used[i] = make([]bool, m)
	}

	// Backtrack all states that can lead to E with shortest path
	// We'll do a multi-end reverse search from all (ex, ey, d) with dist == best
	rev := []state{}
	for d := 0; d < 4; d++ {
		if dist[ex][ey][d] == best {
			rev = append(rev, state{ex, ey, d})
		}
	}

	vis := make([][][]bool, n)
	for i := range vis {
		vis[i] = make([][]bool, m)
		for j := range vis[i] {
			vis[i][j] = make([]bool, 4)
		}
	}
	for _, s := range rev {
		vis[s.x][s.y][s.d] = true
	}

	for len(rev) > 0 {
		u := rev[len(rev)-1]
		rev = rev[:len(rev)-1]
		used[u.x][u.y] = true

		costU := dist[u.x][u.y][u.d]

		// From u came either from rotation or from forward step
		// Check rotations that could have led here
		for _, pd := range []int{(u.d+1)%4, (u.d+3)%4} {
			if dist[u.x][u.y][pd] == costU - 1000 {
				if !vis[u.x][u.y][pd] {
					vis[u.x][u.y][pd] = true
					rev = append(rev, state{u.x, u.y, pd})
				}
			}
		}

		// Check forward step
		px, py := u.x - dx[u.d], u.y - dy[u.d]
		if px >= 0 && px < n && py >= 0 && py < m && grid[px][py] != '#' {
			if dist[px][py][u.d] == costU - 1 {
				if !vis[px][py][u.d] {
					vis[px][py][u.d] = true
					rev = append(rev, state{px, py, u.d})
				}
			}
		}
	}

	cnt := 0
	for i := range used {
		for j := range used[i] {
			if used[i][j] && grid[i][j] != '#' {
				cnt++
			}
		}
	}

	fmt.Println(cnt)
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
