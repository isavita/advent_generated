
use std::collections::HashSet;
use std::fs;

#[derive(Debug, Clone, Copy)]
enum Fold {
    X(i32),
    Y(i32),
}

fn parse_input(input: &str) -> (HashSet<(i32, i32)>, Vec<Fold>) {
    let mut dots = HashSet::new();
    let mut folds = Vec::new();
    let mut parsing_dots = true;

    for line in input.lines() {
        if line.is_empty() {
            parsing_dots = false;
            continue;
        }

        if parsing_dots {
            let coords: Vec<i32> = line.split(',').map(|s| s.parse().unwrap()).collect();
            dots.insert((coords[0], coords[1]));
        } else {
            let parts: Vec<&str> = line.split('=').collect();
            let value = parts[1].parse().unwrap();
            let fold = if parts[0].ends_with('x') {
                Fold::X(value)
            } else {
                Fold::Y(value)
            };
            folds.push(fold);
        }
    }

    (dots, folds)
}

fn fold_dots(dots: &HashSet<(i32, i32)>, fold: Fold) -> HashSet<(i32, i32)> {
    let mut new_dots = HashSet::new();
    for &(x, y) in dots {
        match fold {
            Fold::X(fold_x) => {
                if x > fold_x {
                    new_dots.insert((fold_x - (x - fold_x), y));
                } else {
                    new_dots.insert((x, y));
                }
            }
            Fold::Y(fold_y) => {
                if y > fold_y {
                    new_dots.insert((x, fold_y - (y - fold_y)));
                } else {
                    new_dots.insert((x, y));
                }
            }
        }
    }
    new_dots
}

fn print_dots(dots: &HashSet<(i32, i32)>) {
    if dots.is_empty() {
        return;
    }
    let max_x = dots.iter().map(|&(x, _)| x).max().unwrap();
    let max_y = dots.iter().map(|&(_, y)| y).max().unwrap();

    for y in 0..=max_y {
        for x in 0..=max_x {
            if dots.contains(&(x, y)) {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }
}


fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let (mut dots, folds) = parse_input(&input);

    // part 1
    dots = fold_dots(&dots, folds[0]);
    println!("Part 1: {}", dots.len());

    // part 2
    let mut dots = parse_input(&input).0;
     for &fold in folds.iter() {
        dots = fold_dots(&dots, fold);
    }
    println!("Part 2:");
    print_dots(&dots);

}
