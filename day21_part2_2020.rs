
use std::collections::{HashMap, HashSet};
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut allergen_map: HashMap<&str, HashSet<&str>> = HashMap::new();
    let mut ingredient_count: HashMap<&str, usize> = HashMap::new();

    for line in input.lines() {
        let parts: Vec<&str> = line.split(" (contains ").collect();
        let ingredients: HashSet<&str> = parts[0].split(' ').collect();
        let allergens: HashSet<&str> = parts[1][..parts[1].len() - 1].split(", ").collect();

        for allergen in allergens.iter() {
            if let Some(entry) = allergen_map.get_mut(allergen) {
                *entry = entry.intersection(&ingredients).cloned().collect();
            } else {
                allergen_map.insert(allergen, ingredients.clone());
            }
        }

        for ingredient in ingredients.iter() {
            *ingredient_count.entry(ingredient).or_insert(0) += 1;
        }
    }

    let mut inert_ingredients: HashSet<&str> = HashSet::new();
    for (_, ingredients) in allergen_map.iter() {
        inert_ingredients.extend(ingredients);
    }

    let mut count = 0;
    for (ingredient, &num) in ingredient_count.iter() {
        if !inert_ingredients.contains(ingredient) {
            count += num;
        }
    }

    println!("{}", count);

    let mut dangerous_list: Vec<(&str, &str)> = Vec::new();
    while !allergen_map.is_empty() {
        let mut found_allergen: Option<&str> = None;
        for (&allergen, ingredients) in allergen_map.iter() {
            if ingredients.len() == 1 {
                found_allergen = Some(allergen);
                let ingredient = *ingredients.iter().next().unwrap();
                dangerous_list.push((allergen, ingredient));
                break;
            }
        }

        if let Some(allergen) = found_allergen {
            allergen_map.remove(allergen);
            for (_, ingredients) in allergen_map.iter_mut() {
                ingredients.remove(dangerous_list.last().unwrap().1);
            }
        }
    }

    dangerous_list.sort_by_key(|&(allergen, _)| allergen);
    let result: Vec<&str> = dangerous_list.iter().map(|&(_, ingredient)| ingredient).collect();
    println!("{}", result.join(","));
}
