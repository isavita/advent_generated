package main

import (
	"fmt"
	"math/big"
	"os"
	"regexp"
	"strings"
)

var zero = big.NewRat(0, 1)

type RatVec3 struct {
	X *big.Rat
	Y *big.Rat
	Z *big.Rat
}

func (vec *RatVec3) Add(other *RatVec3) *RatVec3 {
	return &RatVec3{
		X: add(vec.X, other.X),
		Y: add(vec.Y, other.Y),
		Z: add(vec.Z, other.Z),
	}
}

func (vec *RatVec3) Subtract(other *RatVec3) *RatVec3 {
	return &RatVec3{
		X: sub(vec.X, other.X),
		Y: sub(vec.Y, other.Y),
		Z: sub(vec.Z, other.Z),
	}
}

func (vec *RatVec3) Multiply(s *big.Rat) *RatVec3 {
	return &RatVec3{
		X: mul(vec.X, s),
		Y: mul(vec.Y, s),
		Z: mul(vec.Z, s),
	}
}

func (vec *RatVec3) Divide(s *big.Rat) *RatVec3 {
	return &RatVec3{
		X: quo(vec.X, s),
		Y: quo(vec.Y, s),
		Z: quo(vec.Z, s),
	}
}

func (vec *RatVec3) Cross(other *RatVec3) *RatVec3 {
	return &RatVec3{
		X: sub(mul(vec.Y, other.Z), mul(vec.Z, other.Y)),
		Y: sub(mul(vec.Z, other.X), mul(vec.X, other.Z)),
		Z: sub(mul(vec.X, other.Y), mul(vec.Y, other.X)),
	}
}

func (vec *RatVec3) Dot(other *RatVec3) *big.Rat {
	return add(mul(vec.X, other.X), add(mul(vec.Y, other.Y), mul(vec.Z, other.Z)))
}

type HailStone struct {
	p *RatVec3
	v *RatVec3
}

func (stone *HailStone) Subtract(other *HailStone) *HailStone {
	return &HailStone{
		p: stone.p.Subtract(other.p),
		v: stone.v.Subtract(other.v),
	}
}

func readFile(fileName string) []string {
	file, err := os.ReadFile(fileName)
	if err != nil {
		panic(err)
	}

	return strings.Split(strings.TrimSpace(string(file)), "\n")
}

func main() {
	input := readFile("input.txt")
	fmt.Println(solve(input))
}

func solve(input []string) string {
	hailStones := readInput(input[:3])
	s1 := hailStones[1]
	s2 := hailStones[2]
	ref1 := s1.Subtract(&hailStones[0])
	ref2 := s2.Subtract(&hailStones[0])

	t1 := intersectionTime(ref2, ref1)
	t2 := intersectionTime(ref1, ref2)

	rock1 := s1.p.Add(s1.v.Multiply(t1))
	rock2 := s2.p.Add(s2.v.Multiply(t2))

	rp := rock1.Subtract(rock2.Subtract(rock1).Divide(sub(t2, t1)).Multiply(t1))
	return add(rp.X, add(rp.Y, rp.Z)).FloatString(0)
}

func readInput(input []string) []HailStone {
	res := make([]HailStone, len(input))

	for i, line := range input {
		res[i] = readLine(line)
	}

	return res
}

func readLine(line string) HailStone {
	re := regexp.MustCompile(`-*\d+`)
	match := re.FindAllString(line, -1)
	m := make([]int, 6)
	for i, s := range match {
		m[i] = toInt(s)
	}

	return HailStone{
		p: &RatVec3{
			X: big.NewRat(int64(m[0]), 1),
			Y: big.NewRat(int64(m[1]), 1),
			Z: big.NewRat(int64(m[2]), 1),
		},
		v: &RatVec3{
			X: big.NewRat(int64(m[3]), 1),
			Y: big.NewRat(int64(m[4]), 1),
			Z: big.NewRat(int64(m[5]), 1),
		},
	}
}

func toInt(s string) int {
	i, ok := new(big.Int).SetString(s, 10)
	if !ok {
		panic("Error converting string to int")
	}
	return int(i.Int64())
}

func countIntersections(stones []HailStone, start *big.Rat, end *big.Rat) int {
	count := 0

	for i := range stones {
		for j := range stones[i+1:] {
			x, y, t1, t2, ok := doMatch2D(&stones[i], &stones[i+j+1])
			if ok && t1.Cmp(zero) > 0 && t2.Cmp(zero) >= 0 && x.Cmp(start) >= 0 && y.Cmp(start) >= 0 && x.Cmp(end) <= 0 && y.Cmp(end) <= 0 {
				count++
			}
		}
	}

	return count
}

func doMatch2D(a *HailStone, b *HailStone) (*big.Rat, *big.Rat, *big.Rat, *big.Rat, bool) {
	m1 := quo(a.v.Y, a.v.X)
	m2 := quo(b.v.Y, b.v.X)

	if m1.Cmp(m2) == 0 {
		return nil, nil, nil, nil, false
	}

	x := quo(sub(sub(mul(m1, a.p.X), a.p.Y), sub(mul(m2, b.p.X), b.p.Y)), sub(m1, m2))
	y := add(mul(m1, sub(x, a.p.X)), a.p.Y)
	t1 := quo(sub(x, a.p.X), a.v.X)
	t2 := quo(sub(x, b.p.X), b.v.X)

	return x, y, t1, t2, true
}

func intersectionTime(r *HailStone, s *HailStone) *big.Rat {
	plane := r.p.Cross(r.p.Add(r.v))
	return quo(mul(s.p.Dot(plane), big.NewRat(-1, 1)), s.v.Dot(plane))
}

func r(rat *big.Rat) *big.Rat {
	return new(big.Rat).Set(rat)
}

func add(a *big.Rat, b *big.Rat) *big.Rat {
	c := r(a)
	return c.Add(c, b)
}

func sub(a *big.Rat, b *big.Rat) *big.Rat {
	c := r(a)
	return c.Sub(c, b)
}

func mul(a *big.Rat, b *big.Rat) *big.Rat {
	c := r(a)
	return c.Mul(c, b)
}

func quo(a *big.Rat, b *big.Rat) *big.Rat {
	c := r(a)
	return c.Quo(c, b)
}