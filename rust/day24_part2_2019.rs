
use std::fs;

const SIDE: usize = 5;
const SQUARE: usize = SIDE * SIDE;

fn parse() -> [bool; SQUARE] {
    let mut res = [false; SQUARE];

    let contents = fs::read_to_string("input.txt").expect("Failed to read file");
    let mut row = 0;
    for line in contents.lines() {
        for (col, c) in line.chars().enumerate() {
            if c == '#' {
                res[row * SIDE + col] = true;
            }
        }
        row += 1;
    }

    res
}

type Space = std::collections::HashMap<i32, [bool; SQUARE]>;

fn main() {
    let input = parse();

    let mut space = Space::new();
    space.insert(0, input);

    for _ in 0..200 {
        space = next2(&space);
    }

    let mut count = 0;
    for grid in space.values() {
        for &cell in grid {
            if cell {
                count += 1;
            }
        }
    }
    println!("{}", count);
}

fn next2(space: &Space) -> Space {
    let mut new_space = Space::new();

    let (min_level, max_level) = min_max_level(&space);

    for level in min_level - 1..=max_level + 1 {
        let mut new_grid = [false; SQUARE];

        for cell in 0..SQUARE {
            if cell == 12 {
                continue;
            }

            let row = cell / SIDE;
            let col = cell % SIDE;
            let mut neighbours = 0;

            if row == 0 && infested(&space, level - 1, 7) {
                neighbours += 1;
            }

            if col == 0 && infested(&space, level - 1, 11) {
                neighbours += 1;
            }

            if col == 4 && infested(&space, level - 1, 13) {
                neighbours += 1;
            }

            if row == 4 && infested(&space, level - 1, 17) {
                neighbours += 1;
            }

            if cell == 7 {
                for i in 0..SIDE {
                    if infested(&space, level + 1, i) {
                        neighbours += 1;
                    }
                }
            }

            if cell == 11 {
                for i in 0..SIDE {
                    if infested(&space, level + 1, 5 * i) {
                        neighbours += 1;
                    }
                }
            }

            if cell == 13 {
                for i in 0..SIDE {
                    if infested(&space, level + 1, 5 * i + SIDE - 1) {
                        neighbours += 1;
                    }
                }
            }

            if cell == 17 {
                for i in 0..SIDE {
                    if infested(&space, level + 1, (SIDE - 1) * SIDE + i) {
                        neighbours += 1;
                    }
                }
            }

            if row > 0 && cell != 17 && infested(&space, level, cell - SIDE) {
                neighbours += 1;
            }

            if col > 0 && cell != 13 && infested(&space, level, cell - 1) {
                neighbours += 1;
            }

            if col < SIDE - 1 && cell != 11 && infested(&space, level, cell + 1) {
                neighbours += 1;
            }

            if row < SIDE - 1 && cell != 7 && infested(&space, level, cell + SIDE) {
                neighbours += 1;
            }

            if infested(&space, level, cell) && neighbours != 1 {
                new_grid[cell] = false;
            } else if !infested(&space, level, cell) && (neighbours == 1 || neighbours == 2) {
                new_grid[cell] = true;
            } else {
                new_grid[cell] = infested(&space, level, cell);
            }
        }

        new_space.insert(level, new_grid);
    }

    clean(&mut new_space);

    new_space
}

fn clean(space: &mut Space) {
    let (min, max) = min_max_level(&space);

    let mut count_min = 0;
    let mut count_max = 0;
    for &cell in &space[&min] {
        if cell {
            count_min += 1;
        }
    }
    for &cell in &space[&max] {
        if cell {
            count_max += 1;
        }
    }

    if count_min == 0 {
        space.remove(&min);
    }
    if count_max == 0 {
        space.remove(&max);
    }
}

fn infested(space: &Space, level: i32, cell: usize) -> bool {
    if let Some(grid) = space.get(&level) {
        return grid[cell];
    }
    false
}

fn min_max_level(space: &Space) -> (i32, i32) {
    let mut min = 999999;
    let mut max = -999999;
    for level in space.keys() {
        if *level < min {
            min = *level;
        }
        if *level > max {
            max = *level;
        }
    }
    (min, max)
}
