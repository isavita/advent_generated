
import os
import strings

struct IngredientCount {
mut:
	name  string
	count int
}

struct AllergenInfo {
mut:
	name       string
	candidates []string
	assigned   string
}

fn main() {
	mut ingredient_counts := map[string]int{}
	mut allergen_map := map[string][]string{}

	lines := os.read_lines('input.txt') or { panic(err) }
	for line in lines {
		parts := line.split(' (contains ')
		if parts.len != 2 {
			continue
		}
		ingredients := parts[0].split(' ')
		allergens := parts[1].trim_right(')').split(', ')
		for ing in ingredients {
			ingredient_counts[ing]++
		}
		for al in allergens {
			if al !in allergen_map {
				allergen_map[al] = ingredients.clone()
			} else {
				mut new_cand := []string{}
				for c in allergen_map[al] {
					if c in ingredients {
						new_cand << c
					}
				}
				allergen_map[al] = new_cand
			}
		}
	}

	mut safe_count := 0
	for ing, cnt in ingredient_counts {
		mut ok := true
		for _, v in allergen_map {
			if ing in v {
				ok = false
				break
			}
		}
		if ok {
			safe_count += cnt
		}
	}
	println(safe_count)

	mut allergens := []AllergenInfo{}
	for k, v in allergen_map {
		allergens << AllergenInfo{k, v, ''}
	}

	mut assigned := 0
	for assigned < allergens.len {
		for mut al in allergens {
			if al.assigned == '' && al.candidates.len == 1 {
				al.assigned = al.candidates[0]
				assigned++
				for mut o in allergens {
					if o.assigned == '' {
						o.candidates = o.candidates.filter(it != al.assigned)
					}
				}
			}
		}
	}

	allergens.sort(a.name < b.name)
	mut parts := []string{}
	for al in allergens {
		parts << al.assigned
	}
	println(parts.join(','))
}
