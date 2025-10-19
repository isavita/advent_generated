
import os
import arrays
import strings

const max_line_len = 1024
const max_items = 100
const max_unique_ingredients = 500
const max_unique_allergens = 50

struct Ingredient {
mut:
	name                   string
	count                  int
	potentially_allergenic bool
}

struct Allergen {
mut:
	name                string
	potential_ingredients []string
}

fn main() {
	content := os.read_file('input.txt') or { panic('cannot read input.txt') }
	lines := content.split_into_lines()

	mut ingredients := []Ingredient{}
	mut allergens := []Allergen{cap: max_unique_allergens}

	for line in lines {
		parts := line.split(' (contains ')
		if parts.len != 2 { continue }
		ingredient_names := parts[0].split(' ')
		allergen_names := parts[1].replace(')', '').split(', ')

		for name in ingredient_names {
			mut idx := -1
			for i, ing in ingredients {
				if ing.name == name {
					idx = i
					break
				}
			}
			if idx == -1 {
				ingredients << Ingredient{name, 1, false}
			} else {
				ingredients[idx].count++
			}
		}

		for a in allergen_names {
			mut allergen_idx := -1
			for i, al in allergens {
				if al.name == a {
					allergen_idx = i
					break
				}
			}
			if allergen_idx == -1 {
				allergens << Allergen{a, ingredient_names.clone()}
			} else {
				mut filtered := []string{}
				for p in allergens[allergen_idx].potential_ingredients {
					if p in ingredient_names {
						filtered << p
					}
				}
				allergens[allergen_idx].potential_ingredients = filtered
			}
		}
	}

	for al in allergens {
		for p in al.potential_ingredients {
			for mut i in ingredients {
				if i.name == p {
					i.potentially_allergenic = true
					break
							}
			}
		}
	}

	mut safe := 0
	for i in ingredients {
		if !i.potentially_allergenic {
			safe += i.count
		}
	}
	println(safe)
}
