
use std::collections::{HashMap, HashSet};
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read input file");
    let mut allergen_map: HashMap<&str, HashSet<&str>> = HashMap::new();
    let mut ingredient_count: HashMap<&str, usize> = HashMap::new();
    
    for line in input.lines() {
        let parts: Vec<&str> = line.split(" (contains ").collect();
        let ingredients: HashSet<&str> = parts[0].split(' ').collect();
        let allergens: HashSet<&str> = parts[1].trim_end_matches(')').split(", ").collect();
        
        for ingredient in ingredients.iter() {
            *ingredient_count.entry(ingredient).or_insert(0) += 1;
        }
        
        for allergen in allergens.iter() {
            if let Some(existing) = allergen_map.get_mut(allergen) {
                *existing = existing.intersection(&ingredients).cloned().collect();
            } else {
                allergen_map.insert(allergen, ingredients.clone());
            }
        }
    }
    
    let mut allergen_ingredients: HashSet<&str> = HashSet::new();
    for ingredients in allergen_map.values() {
        allergen_ingredients.extend(ingredients);
    }
    
    let mut count = 0;
    for (ingredient, &c) in ingredient_count.iter() {
        if !allergen_ingredients.contains(ingredient) {
            count += c;
        }
    }
    
    println!("{}", count);
}
