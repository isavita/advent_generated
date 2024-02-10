package main

import (
	"container/heap"
	"fmt"
	"image"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

var Neighbors4 = []image.Point{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}
var re = regexp.MustCompile(`-x(\d+)-y(\d+)`)

type node struct {
	used, avail int
}

type Item struct {
	Obj      interface{}
	Priority int
}

type PQ []*Item

func (pq PQ) Len() int            { return len(pq) }
func (pq PQ) Less(i, j int) bool  { return pq[i].Priority > pq[j].Priority }
func (pq PQ) Swap(i, j int)       { pq[i], pq[j] = pq[j], pq[i] }
func (pq *PQ) Push(x interface{}) { *pq = append(*pq, x.(*Item)) }
func (pq *PQ) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = nil
	*pq = old[:n-1]
	return item
}

func main() {
	nodes := make(map[image.Point]node)
	input := strings.Split(readAll(), "\n")[2:]
	for i := range input {
		f := strings.Fields(input[i])
		matches := re.FindAllStringSubmatch(f[0], -1)[0]
		var n node
		m1, _ := strconv.Atoi(matches[1])
		m2, _ := strconv.Atoi(matches[2])
		p := image.Pt(m1, m2)
		n.used, _ = strconv.Atoi(f[2][:len(f[2])-1])
		n.avail, _ = strconv.Atoi(f[3][:len(f[3])-1])
		nodes[p] = n
	}
	fmt.Println(minmoves(nodes))
}

func minmoves(nodes map[image.Point]node) int {
	w, _ := dim(nodes)
	goal := image.Pt(w, 0)
	hole, err := findHole(nodes)
	if err != nil {
		log.Fatal(err)
	}
	// move hole to adjacent to goal
	// swap goal and hole
	// repeat until goal at origin
	var sum int
	for goal != image.Pt(0, 0) {
		next := goal.Add(image.Pt(-1, 0))
		m, err := moves(nodes, goal, hole, next)
		if err != nil {
			log.Fatal(err)
		}
		sum += m
		hole = next
		m, err = moves(nodes, goal, goal, hole)
		if err != nil {
			log.Fatal(err)
		}
		sum += m
		goal, hole = hole, goal
	}
	return sum
}

func findHole(nodes map[image.Point]node) (image.Point, error) {
	for p, n := range nodes {
		if n.used == 0 {
			return p, nil
		}
	}
	return image.Pt(0, 0), fmt.Errorf("no hole")
}

func moves(nodes map[image.Point]node, goal, from, to image.Point) (int, error) {
	w, h := dim(nodes)
	depth := map[image.Point]int{from: 0}
	pq := PQ{&Item{from, 0}}
	heap.Init(&pq)
	for pq.Len() > 0 {
		p := heap.Pop(&pq).(*Item).Obj.(image.Point)
		if p == to {
			return depth[p], nil
		}
		currdepth := depth[p] + 1
		for _, n := range Neighbors4 {
			next := p.Add(n)
			if next.X < 0 || next.Y < 0 || next.X > w || next.Y > h || nodes[next].used > 400 || next == goal {
				continue
			}
			if d, ok := depth[next]; !ok || currdepth < d {
				depth[next] = currdepth
				heap.Push(&pq, &Item{next, -currdepth})
			}
		}
	}
	return -1, fmt.Errorf("no possible path")
}

func dim(m map[image.Point]node) (w int, h int) {
	for p := range m {
		if p.X > w {
			w = p.X
		}
		if p.Y > h {
			h = p.Y
		}
	}
	return
}

func readAll() string {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	return strings.TrimSpace(string(input))
}