package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)

type Cart struct {
	x, y    int
	dx, dy  int
	turns   int
	crashed bool
}

func main() {
	track, carts := parseInput("input.txt")
	x, y := simulate(track, carts)
	fmt.Printf("%d,%d\n", x, y)
}

func parseInput(filename string) ([][]rune, []*Cart) {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)

	var track [][]rune
	var carts []*Cart
	y := 0
	for scanner.Scan() {
		line := []rune(scanner.Text())
		for x, r := range line {
			switch r {
			case '^':
				carts = append(carts, &Cart{x, y, 0, -1, 0, false})
				line[x] = '|'
			case 'v':
				carts = append(carts, &Cart{x, y, 0, 1, 0, false})
				line[x] = '|'
			case '<':
				carts = append(carts, &Cart{x, y, -1, 0, 0, false})
				line[x] = '-'
			case '>':
				carts = append(carts, &Cart{x, y, 1, 0, 0, false})
				line[x] = '-'
			}
		}
		track = append(track, line)
		y++
	}
	return track, carts
}

func simulate(track [][]rune, carts []*Cart) (int, int) {
	for {
		sort.Slice(carts, func(i, j int) bool {
			if carts[i].y == carts[j].y {
				return carts[i].x < carts[j].x
			}
			return carts[i].y < carts[j].y
		})

		for _, cart := range carts {
			if cart.crashed {
				continue
			}

			cart.x += cart.dx
			cart.y += cart.dy

			switch track[cart.y][cart.x] {
			case '+':
				cart.turn()
			case '/':
				cart.dx, cart.dy = -cart.dy, -cart.dx
			case '\\':
				cart.dx, cart.dy = cart.dy, cart.dx
			}

			for _, other := range carts {
				if other != cart && !other.crashed && cart.x == other.x && cart.y == other.y {
					return cart.x, cart.y
				}
			}
		}
	}
}

func (c *Cart) turn() {
	switch c.turns % 3 {
	case 0: // Turn left
		c.dx, c.dy = -c.dy, c.dx
	case 2: // Turn right
		c.dx, c.dy = c.dy, -c.dx
	}
	c.turns++
}
