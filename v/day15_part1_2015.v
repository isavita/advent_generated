import os

struct Ingredient {
	name      string
	capacity  int
	durability int
	flavor    int
	texture   int
	calories  int
}

fn parse_ingredient(line string) Ingredient {
	parts := line.split(': ')
	name := parts[0]
	props := parts[1].split(', ')
	mut capacity := 0
	mut durability := 0
	mut flavor := 0
	mut texture := 0
	mut calories := 0

	for prop in props {
		kv := prop.split(' ')
		value := kv[1].int()
		match kv[0] {
			'capacity' { capacity = value }
			'durability' { durability = value }
			'flavor' { flavor = value }
			'texture' { texture = value }
			'calories' { calories = value }
			else {}
		}
	}
	return Ingredient{name, capacity, durability, flavor, texture, calories}
}

fn calculate_score(ingredients []Ingredient, amounts []int) int {
	mut capacity := 0
	mut durability := 0
	mut flavor := 0
	mut texture := 0

	for i in 0 .. ingredients.len {
		capacity += amounts[i] * ingredients[i].capacity
		durability += amounts[i] * ingredients[i].durability
		flavor += amounts[i] * ingredients[i].flavor
		texture += amounts[i] * ingredients[i].texture
	}
	capacity = if capacity < 0 { 0 } else { capacity }
	durability = if durability < 0 { 0 } else { durability }
	flavor = if flavor < 0 { 0 } else { flavor }
	texture = if texture < 0 { 0 } else { texture }

	return capacity * durability * flavor * texture
}

fn find_best_score(ingredients []Ingredient) int {
	mut best_score := 0
	for i in 0 .. 101 {
		for j in 0 .. 101 - i {
			for k in 0 .. 101 - i - j {
				l := 100 - i - j - k
				if l < 0 { continue }
				amounts := [i, j, k, l]
				score := calculate_score(ingredients, amounts)
				if score > best_score {
					best_score = score
				}
			}
		}
	}
	return best_score
}

fn main() {
	mut ingredients := []Ingredient{}
	lines := os.read_lines('input.txt') or { panic(err) }
	for line in lines {
		ingredients << parse_ingredient(line)
	}
	best_score := find_best_score(ingredients)
	println(best_score)
}