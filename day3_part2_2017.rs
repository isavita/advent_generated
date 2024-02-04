
use std::fs;

fn main() {
    let data = fs::read_to_string("input.txt").expect("File reading error");
    let target: i32 = data.trim().parse().expect("File parsing error");

    let mut grid = std::collections::HashMap::new();
    grid.insert([0, 0], 1);

    let (mut x, mut y) = (0, 0);
    let (mut dx, mut dy) = (0, -1);

    loop {
        if x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y) {
            let temp = dx;
            dx = -dy;
            dy = temp;
        }

        x += dx;
        y += dy;

        let mut value = 0;
        for dx in -1..=1 {
            for dy in -1..=1 {
                value += grid.get(&[x + dx, y + dy]).unwrap_or(&0);
            }
        }
        grid.insert([x, y], value);

        if value > target {
            println!("{}", value);
            break;
        }
    }
}
