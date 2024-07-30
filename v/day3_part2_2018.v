import os

struct Claim {
	id     int
	x      int
	y      int
	width  int
	height int
}

fn main() {
	claims := read_claims('input.txt') or { return }
	mut fabric := [][]int{len: 1000, init: []int{len: 1000, init: 0}}

	for claim in claims {
		for y in claim.y .. claim.y + claim.height {
			for x in claim.x .. claim.x + claim.width {
				fabric[y][x]++
			}
		}
	}

	for claim in claims {
		mut overlap := false
		for y in claim.y .. claim.y + claim.height {
			for x in claim.x .. claim.x + claim.width {
				if fabric[y][x] > 1 {
					overlap = true
					break
				}
			}
			if overlap {
				break
			}
		}
		if !overlap {
			println(claim.id)
			return
		}
	}
}

fn read_claims(filename string) ![]Claim {
	mut claims := []Claim{}
	lines := os.read_lines(filename) or { return error('failed to read file') }

	for line in lines {
		parts := line.split(' ')
		id := parts[0][1..].int()
		coords := parts[2][..parts[2].len - 1].split(',')
		x := coords[0].int()
		y := coords[1].int()
		dims := parts[3].split('x')
		width := dims[0].int()
		height := dims[1].int()
		claims << Claim{id, x, y, width, height}
	}
	return claims
}