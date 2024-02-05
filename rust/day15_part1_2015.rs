
use std::fs;

struct Ingredient {
    name: String,
    capacity: i32,
    durability: i32,
    flavor: i32,
    texture: i32,
}

fn main() {
    let ingredients = read_ingredients("input.txt").unwrap();
    let max_score = find_max_score(&ingredients, 100);
    println!("{}", max_score);
}

fn read_ingredients(filename: &str) -> Result<Vec<Ingredient>, std::io::Error> {
    let contents = fs::read_to_string(filename)?;
    let mut ingredients = Vec::new();

    for line in contents.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 11 {
            continue;
        }

        let capacity: i32 = parts[2][..parts[2].len()-1].parse().unwrap();
        let durability: i32 = parts[4][..parts[4].len()-1].parse().unwrap();
        let flavor: i32 = parts[6][..parts[6].len()-1].parse().unwrap();
        let texture: i32 = parts[8][..parts[8].len()-1].parse().unwrap();

        ingredients.push(Ingredient {
            name: parts[0].to_string(),
            capacity,
            durability,
            flavor,
            texture,
        });
    }

    Ok(ingredients)
}

fn find_max_score(ingredients: &Vec<Ingredient>, total_teaspoons: i32) -> i32 {
    calculate_max_score(ingredients, 0, total_teaspoons, &mut vec![])
}

fn calculate_max_score(ingredients: &Vec<Ingredient>, index: usize, remaining: i32, teaspoons: &mut Vec<i32>) -> i32 {
    if index == ingredients.len() - 1 {
        teaspoons.push(remaining);
        return score(ingredients, &teaspoons);
    }

    let mut max_score = 0;
    for i in 0..=remaining {
        let score = calculate_max_score(ingredients, index + 1, remaining - i, &mut {
            let mut new_teaspoons = teaspoons.clone();
            new_teaspoons.push(i);
            new_teaspoons
        });
        if score > max_score {
            max_score = score;
        }
    }
    max_score
}

fn score(ingredients: &Vec<Ingredient>, teaspoons: &Vec<i32>) -> i32 {
    let mut capacity = 0;
    let mut durability = 0;
    let mut flavor = 0;
    let mut texture = 0;

    for (i, ingredient) in ingredients.iter().enumerate() {
        capacity += ingredient.capacity * teaspoons[i];
        durability += ingredient.durability * teaspoons[i];
        flavor += ingredient.flavor * teaspoons[i];
        texture += ingredient.texture * teaspoons[i];
    }

    if capacity < 0 {
        capacity = 0;
    }
    if durability < 0 {
        durability = 0;
    }
    if flavor < 0 {
        flavor = 0;
    }
    if texture < 0 {
        texture = 0;
    }

    capacity * durability * flavor * texture
}
