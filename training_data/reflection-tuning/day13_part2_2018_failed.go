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
	turn    int
	crashed bool
}

func main() {
	track, carts := parseInput()
	firstCrash := true
	tick := 0

	for len(carts) > 1 {
		tick++
		sort.Slice(carts, func(i, j int) bool {
			if carts[i].y == carts[j].y {
				return carts[i].x < carts[j].x
			}
			return carts[i].y < carts[j].y
		})

		positions := make(map[int]map[int]bool)
		for i := range carts {
			if carts[i].crashed {
				continue
			}

			carts[i].x += carts[i].dx
			carts[i].y += carts[i].dy

			if positions[carts[i].y] == nil {
				positions[carts[i].y] = make(map[int]bool)
			}
			if positions[carts[i].y][carts[i].x] {
				if firstCrash {
					fmt.Printf("Part 1: %d,%d\n", carts[i].x, carts[i].y)
					firstCrash = false
				}
				carts[i].crashed = true
				for j := range carts {
					if carts[j].x == carts[i].x && carts[j].y == carts[i].y {
						carts[j].crashed = true
					}
				}
			} else {
				positions[carts[i].y][carts[i].x] = true
			}

			switch track[carts[i].y][carts[i].x] {
			case '/':
				carts[i].dx, carts[i].dy = -carts[i].dy, -carts[i].dx
			case '\\':
				carts[i].dx, carts[i].dy = carts[i].dy, carts[i].dx
			case '+':
				switch carts[i].turn {
				case 0: // Left
					carts[i].dx, carts[i].dy = carts[i].dy, -carts[i].dx
				case 2: // Right
					carts[i].dx, carts[i].dy = -carts[i].dy, carts[i].dx
				}
				carts[i].turn = (carts[i].turn + 1) % 3
			}
		}

		newCarts := make([]*Cart, 0)
		for _, cart := range carts {
			if !cart.crashed {
				newCarts = append(newCarts, cart)
			}
		}
		carts = newCarts
	}

	if len(carts) == 1 {
		fmt.Printf("Part 2: %d,%d\n", carts[0].x, carts[0].y)
	}
}

func parseInput() ([]string, []*Cart) {
	scanner := bufio.NewScanner(os.Stdin)
	var track []string
	var carts []*Cart

	for y := 0; scanner.Scan(); y++ {
		line := scanner.Text()
		track = append(track, line)
		for x, ch := range line {
			var cart *Cart
			switch ch {
			case '^':
				cart = &Cart{x, y, 0, -1, 0, false}
			case 'v':
				cart = &Cart{x, y, 0, 1, 0, false}
			case '<':
				cart = &Cart{x, y, -1, 0, 0, false}
			case '>':
				cart = &Cart{x, y, 1, 0, 0, false}
			}
			if cart != nil {
				carts = append(carts, cart)
			}
		}
	}

	return track, carts
}
