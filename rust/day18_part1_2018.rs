use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut area: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();

    for _ in 0..10 {
        let mut new_area = area.clone();

        for y in 0..area.len() {
            for x in 0..area[y].len() {
                let mut tree_count = 0;
                let mut lumberyard_count = 0;

                for dy in -1..=1 {
                    for dx in -1..=1 {
                        if dy == 0 && dx == 0 {
                            continue;
                        }

                        let ny = y as i32 + dy;
                        let nx = x as i32 + dx;

                        if ny >= 0 && ny < area.len() as i32 && nx >= 0 && nx < area[y].len() as i32 {
                            match area[ny as usize][nx as usize] {
                                '|' => tree_count += 1,
                                '#' => lumberyard_count += 1,
                                _ => {}
                            }
                        }
                    }
                }

                match area[y][x] {
                    '.' => {
                        if tree_count >= 3 {
                            new_area[y][x] = '|';
                        }
                    }
                    '|' => {
                        if lumberyard_count >= 3 {
                            new_area[y][x] = '#';
                        }
                    }
                    '#' => {
                        if tree_count == 0 || lumberyard_count == 0 {
                            new_area[y][x] = '.';
                        }
                    }
                    _ => {}
                }
            }
        }

        area = new_area;
    }

    let wooded_acres = area.iter().flatten().filter(|&&c| c == '|').count();
    let lumberyards = area.iter().flatten().filter(|&&c| c == '#').count();

    println!("{}", wooded_acres * lumberyards);
}