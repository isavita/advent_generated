package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)

type Cart struct {
	x, y    int
	dir     byte
	turns   int
	crashed bool
}

func main() {
	track, carts := parseInput("input.txt")
	part1, part2 := simulate(track, carts)
	fmt.Println("Part 1:", part1)
	fmt.Println("Part 2:", part2)
}

func parseInput(filename string) ([][]byte, []*Cart) {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)
	var track [][]byte
	var carts []*Cart
	y := 0
	for scanner.Scan() {
		line := []byte(scanner.Text())
		for x, ch := range line {
			if ch == '^' || ch == 'v' || ch == '<' || ch == '>' {
				carts = append(carts, &Cart{x, y, ch, 0, false})
				if ch == '^' || ch == 'v' {
					line[x] = '|'
				} else {
					line[x] = '-'
				}
			}
		}
		track = append(track, line)
		y++
	}
	return track, carts
}

func simulate(track [][]byte, carts []*Cart) (string, string) {
	part1 := ""
	for len(carts) > 1 {
		sort.Slice(carts, func(i, j int) bool {
			return carts[i].y < carts[j].y || (carts[i].y == carts[j].y && carts[i].x < carts[j].x)
		})
		for _, cart := range carts {
			if cart.crashed {
				continue
			}
			moveCart(cart, track)
			if collision := checkCollision(carts); collision != nil {
				if part1 == "" {
					part1 = fmt.Sprintf("%d,%d", collision.x, collision.y)
				}
				collision.crashed = true
				cart.crashed = true
			}
		}
		carts = removeCollided(carts)
	}
	if len(carts) == 1 {
		return part1, fmt.Sprintf("%d,%d", carts[0].x, carts[0].y)
	}
	return part1, "No carts left"
}

func moveCart(cart *Cart, track [][]byte) {
	switch cart.dir {
	case '^':
		cart.y--
	case 'v':
		cart.y++
	case '<':
		cart.x--
	case '>':
		cart.x++
	}
	switch track[cart.y][cart.x] {
	case '/':
		if cart.dir == '^' {
			cart.dir = '>'
		} else if cart.dir == 'v' {
			cart.dir = '<'
		} else if cart.dir == '<' {
			cart.dir = 'v'
		} else if cart.dir == '>' {
			cart.dir = '^'
		}
	case '\\':
		if cart.dir == '^' {
			cart.dir = '<'
		} else if cart.dir == 'v' {
			cart.dir = '>'
		} else if cart.dir == '<' {
			cart.dir = '^'
		} else if cart.dir == '>' {
			cart.dir = 'v'
		}
	case '+':
		turn := [3]byte{'<', '^', '>'}
		idx := 0
		for i, d := range turn {
			if d == cart.dir {
				idx = i
				break
			}
		}
		switch cart.turns % 3 {
		case 0: // Turn left
			cart.dir = turn[(idx+2)%3]
		case 2: // Turn right
			cart.dir = turn[(idx+1)%3]
		}
		cart.turns++
	}
}

func checkCollision(carts []*Cart) *Cart {
	for i, c1 := range carts {
		if c1.crashed {
			continue
		}
		for j, c2 := range carts {
			if i != j && !c2.crashed && c1.x == c2.x && c1.y == c2.y {
				return c1
			}
		}
	}
	return nil
}

func removeCollided(carts []*Cart) []*Cart {
	newCarts := []*Cart{}
	for _, cart := range carts {
		if !cart.crashed {
			newCarts = append(newCarts, cart)
		}
	}
	return newCarts
}
