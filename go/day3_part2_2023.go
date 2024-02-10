package main

import (
	"fmt"
	"image"
	"os"
	"strings"
)

var Neighbors8 = []image.Point{
	{0, 1}, {0, -1}, {1, 0}, {-1, 0},
	{-1, -1}, {-1, 1}, {1, -1}, {1, 1},
}

type part struct {
	xmin, xmax int
	y          int
	n          int
}

func (p part) valid(grid map[image.Point]byte) bool {
	for x := p.xmin; x <= p.xmax; x++ {
		for _, n := range Neighbors8 {
			c, ok := grid[n.Add(image.Pt(x, p.y))]
			if ok && c != '.' && (c < '0' || c > '9') {
				return true
			}
		}
	}
	return false
}

func main() {
	buf, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(buf))

	grid := map[image.Point]byte{}
	var parts []part
	var curr *part
	for y, line := range strings.Split(input, "\n") {
		if curr != nil {
			parts = append(parts, *curr)
			curr = nil
		}
		for x := 0; x < len(line); x++ {
			c := line[x]
			grid[image.Pt(x, y)] = c
			if c >= '0' && c <= '9' {
				if curr == nil {
					curr = new(part)
					curr.y = y
					curr.xmin = x
					curr.xmax = x
					curr.n = int(c - '0')
				} else {
					curr.n *= 10
					curr.n += int(c - '0')
					curr.xmax = x
				}
			} else if curr != nil {
				parts = append(parts, *curr)
				curr = nil
			}
		}
	}

	partsGrid := map[image.Point]int{}
	for i, p := range parts {
		for x := p.xmin; x <= p.xmax; x++ {
			partsGrid[image.Pt(x, p.y)] = i
		}
	}

	sum := 0
	for p, c := range grid {
		if c == '*' {
			neighborParts := map[int]struct{}{}
			for _, n := range Neighbors8 {
				if i, ok := partsGrid[n.Add(p)]; ok {
					neighborParts[i] = struct{}{}
				}
			}
			if len(neighborParts) == 2 {
				prod := 1
				for i := range neighborParts {
					prod *= parts[i].n
				}
				sum += prod
			}
		}
	}
	fmt.Println(sum)
}