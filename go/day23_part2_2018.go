package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Coordinate struct {
	X, Y, Z int
}

var Zero = Coordinate{X: 0, Y: 0, Z: 0}

func (c Coordinate) Distance(a Coordinate) int {
	return abs(c.X-a.X) + abs(c.Y-a.Y) + abs(c.Z-a.Z)
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

type Bots map[Coordinate][]int

func NewBots(input []string) Bots {
	m := make(Bots)

	for _, data := range input {
		var r int
		var c Coordinate

		_, err := fmt.Sscanf(data, "pos=<%d,%d,%d>, r=%d", &c.X, &c.Y, &c.Z, &r)
		if err != nil {
			panic(err)
		}
		m[c] = append(m[c], r)
	}

	return m
}

func (m Bots) HaveInRange(pos Coordinate) int {
	var sum int

	for c, rs := range m {
		for _, r := range rs {
			if pos.Distance(c) <= r {
				sum++
			}
		}
	}

	return sum
}

func StrongestReachable(bots Bots) int {
	var largestRadius, count int
	var largestPos Coordinate

	for c, rs := range bots {
		for _, r := range rs {
			if r > largestRadius {
				largestPos = c
				largestRadius = r
			}
		}
	}

	for c, rs := range bots {
		if largestPos.Distance(c) <= largestRadius {
			count += len(rs)
		}
	}

	return count
}

func closestSuccess(bots Bots) int {
	var cur, topLeft, bottomRight Coordinate
	zoom := 1 << (strconv.IntSize - 2)

	for {
		zoomedBots := make(Bots)
		best := struct {
			pos   Coordinate
			count int
		}{}

		for c, rs := range bots {
			for _, r := range rs {
				zc := Coordinate{c.X / zoom, c.Y / zoom, c.Z / zoom}
				zoomedBots[zc] = append(zoomedBots[zc], r/zoom)
			}
		}

		for cur.X = topLeft.X; cur.X <= bottomRight.X; cur.X++ {
			for cur.Y = topLeft.Y; cur.Y <= bottomRight.Y; cur.Y++ {
				for cur.Z = topLeft.Z; cur.Z <= bottomRight.Z; cur.Z++ {
					c := zoomedBots.HaveInRange(cur)

					// skip less bots
					if c < best.count {
						continue
					}
					// skip same amount of bots but Distance from Zero is the same or more
					if c == best.count && Zero.Distance(cur) >= Zero.Distance(best.pos) {
						continue
					}
					// more bots or same and closer to Zero
					best.pos, best.count = cur, c
				}
			}
		}

		// zoom in
		topLeft.X, topLeft.Y, topLeft.Z = (best.pos.X-1)<<1, (best.pos.Y-1)<<1, (best.pos.Z-1)<<1
		bottomRight.X, bottomRight.Y, bottomRight.Z = (best.pos.X+1)<<1, (best.pos.Y+1)<<1, (best.pos.Z+1)<<1
		zoom >>= 1

		if zoom == 0 {
			return Zero.Distance(best.pos)
		}
	}
}

func main() {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	bots := NewBots(lines)
	fmt.Println(closestSuccess(bots))
}