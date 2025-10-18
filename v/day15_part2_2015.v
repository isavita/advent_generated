
import os

struct Ingredient {
	name string
	capacity int
	durability int
	flavor int
	texture int
	calories int
}

fn read_ingredients(path string) []Ingredient {
	mut res := []Ingredient{}
	for line in os.read_lines(path) or { return res } {
		parts := line.split(' ')
		res << Ingredient{
			name: parts[0]
			capacity: parts[2].int()
			durability: parts[4].int()
			flavor: parts[6].int()
			texture: parts[8].int()
			calories: parts[10].int()
		}
	}
	return res
}

fn calc(ings []Ingredient, qty []int) int {
	mut c, mut d, mut f, mut t := 0, 0, 0, 0
	for i, ing in ings {
		c += ing.capacity * qty[i]
		d += ing.durability * qty[i]
		f += ing.flavor * qty[i]
		t += ing.texture * qty[i]
	}
	if c < 0 { c = 0 }
	if d < 0 { d = 0 }
	if f < 0 { f = 0 }
	if t < 0 { t = 0 }
	return c * d * f * t
}

fn cals(ings []Ingredient, qty []int) int {
	mut sum := 0
	for i, ing in ings {
		sum += ing.calories * qty[i]
	}
	return sum
}

fn dfs(ings []Ingredient, idx int, rem int, mut qty []int, target int) int {
	if idx == ings.len - 1 {
		qty[idx] = rem
		if cals(ings, qty) == target { return calc(ings, qty) }
		return 0
	}
	mut best := 0
	for i in 0 .. rem + 1 {
		qty[idx] = i
		cur := dfs(ings, idx + 1, rem - i, mut qty, target)
		if cur > best { best = cur }
	}
	return best
}

fn main() {
	ings := read_ingredients('input.txt')
	mut qty := []int{len: ings.len}
	println(dfs(ings, 0, 100, mut qty, 500))
}
