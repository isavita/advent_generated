package main

import (
	"bufio"
	"fmt"
	"os"
)

type point struct{ x, y int }

func main() {
	f, _ := os.Open("input.txt")
	defer f.Close()
	scanner := bufio.NewScanner(f)
	var grid []string
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}

	h, w := len(grid), len(grid[0])
	var S, E point
	walls := make([][]bool, h)
	for i := range walls {
		walls[i] = make([]bool, w)
	}
	var trackCells []point
	for i := 0; i < h; i++ {
		for j := 0; j < w; j++ {
			ch := grid[i][j]
			if ch == 'S' {
				S = point{i, j}
			} else if ch == 'E' {
				E = point{i, j}
			}
			if ch == '#' {
				walls[i][j] = true
			} else {
				trackCells = append(trackCells, point{i, j})
			}
		}
	}

	dirs := []point{{1,0},{-1,0},{0,1},{0,-1}}
	isTrack := func(x,y int)bool{
		return x>=0 && x<h && y>=0 && y<w && !walls[x][y]
	}

	normalDistFrom := func(start point)[][]int {
		dist := make([][]int,h)
		for i:=range dist {
			dist[i] = make([]int,w)
			for j:=range dist[i] {
				dist[i][j] = -1
			}
		}
		dist[start.x][start.y] = 0
		q := []point{start}
		for len(q)>0 {
			cur:=q[0];q=q[1:]
			for _,d:=range dirs {
				nx,ny:=cur.x+d.x,cur.y+d.y
				if nx<0||nx>=h||ny<0||ny>=w{continue}
				if walls[nx][ny]{continue}
				if dist[nx][ny]<0 {
					dist[nx][ny]=dist[cur.x][cur.y]+1
					q=append(q,point{nx,ny})
				}
			}
		}
		return dist
	}

	distFromS:=normalDistFrom(S)
	distFromE:=normalDistFrom(E)
	if distFromS[E.x][E.y]<0 {
		fmt.Println(0)
		return
	}
	normalCost := distFromS[E.x][E.y]

	// We allow cheats up to length 20 ignoring walls.
	// Cheats are uniquely identified by start and end positions.
	// We'll store best cost for each (start,end) pair.
	type pair struct {
		sx,sy,ex,ey int
	}
	cheats := make(map[pair]int)

	for _,startPos:=range trackCells {
		sd:=distFromS[startPos.x][startPos.y]
		if sd<0 {continue}

		// BFS up to 20 steps ignoring walls
		distC := make([][]int,h)
		for i:=range distC {
			distC[i]=make([]int,w)
			for j:=range distC[i] {
				distC[i][j]=-1
			}
		}
		distC[startPos.x][startPos.y]=0
		q:=[]point{startPos}

		for len(q)>0 {
			cur:=q[0];q=q[1:]
			steps:=distC[cur.x][cur.y]
			if steps==20 {continue}
			for _,d:=range dirs {
				nx,ny:=cur.x+d.x,cur.y+d.y
				if nx<0||nx>=h||ny<0||ny>=w{continue}
				// can go through walls here
				if distC[nx][ny]<0 {
					distC[nx][ny]=steps+1
					q=append(q,point{nx,ny})
				}
			}
		}

		// Now distC[x][y] = steps needed to reach (x,y) ignoring walls, or -1 if not reachable
		for x:=0;x<h;x++ {
			for y:=0;y<w;y++ {
				s:=distC[x][y]
				if s>0 && s<=20 && isTrack(x,y) {
					ed:=distFromE[x][y]
					if ed<0 {continue}
					cost := sd+s+ed
					if cost<normalCost {
						key:=pair{startPos.x,startPos.y,x,y}
						old,ok:=cheats[key]
						if !ok||cost<old{
							cheats[key]=cost
						}
					}
				}
			}
		}
	}

	count:=0
	for _,cost:=range cheats {
		saving:=normalCost - cost
		if saving>=100 {
			count++
		}
	}
	fmt.Println(count)
}
