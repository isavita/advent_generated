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
	tracks, carts := parseInput()
	
	for tick := 0; ; tick++ {
		sort.Slice(carts, func(i, j int) bool {
			if carts[i].y == carts[j].y {
				return carts[i].x < carts[j].x
			}
			return carts[i].y < carts[j].y
		})

		for i := range carts {
			if carts[i].crashed {
				continue
			}

			carts[i].x += carts[i].dx
			carts[i].y += carts[i].dy

			switch tracks[carts[i].y][carts[i].x] {
			case '/':
				carts[i].dx, carts[i].dy = -carts[i].dy, -carts[i].dx
			case '\\':
				carts[i].dx, carts[i].dy = carts[i].dy, carts[i].dx
			case '+':
				switch carts[i].turn {
				case 0: // Turn left
					carts[i].dx, carts[i].dy = carts[i].dy, -carts[i].dx
				case 2: // Turn right
					carts[i].dx, carts[i].dy = -carts[i].dy, carts[i].dx
				}
				carts[i].turn = (carts[i].turn + 1) % 3
			}

			for j := range carts {
				if i != j && !carts[j].crashed && carts[i].x == carts[j].x && carts[i].y == carts[j].y {
					fmt.Printf("%d,%d\n", carts[i].x, carts[i].y)
					return
				}
			}
		}
	}
}

func parseInput() ([]string, []*Cart) {
	scanner := bufio.NewScanner(os.Stdin)
	var tracks []string
	var carts []*Cart

	for y := 0; scanner.Scan(); y++ {
		line := scanner.Text()
		tracks = append(tracks, line)
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

	return tracks, carts
}
