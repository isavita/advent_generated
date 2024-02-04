
use std::fs;

fn is_wall(x: i32, y: i32, favorite_number: i32) -> bool {
    let sum = x*x + 3*x + 2*x*y + y + y*y + favorite_number;
    let count = sum.count_ones();
    count % 2 != 0
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let favorite_number: i32 = input.trim().parse().unwrap();

    let mut queue = Vec::new();
    let mut visited = vec![vec![false; 50]; 50];
    queue.push((1, 1, 0));

    while !queue.is_empty() {
        let (x, y, steps) = queue.remove(0);

        if x == 31 && y == 39 {
            println!("{}", steps);
            break;
        }

        visited[y as usize][x as usize] = true;

        if x > 0 && !is_wall(x-1, y, favorite_number) && !visited[y as usize][(x-1) as usize] {
            queue.push((x-1, y, steps + 1));
        }
        if !is_wall(x+1, y, favorite_number) && !visited[y as usize][(x+1) as usize] {
            queue.push((x+1, y, steps + 1));
        }
        if y > 0 && !is_wall(x, y-1, favorite_number) && !visited[(y-1) as usize][x as usize] {
            queue.push((x, y-1, steps + 1));
        }
        if !is_wall(x, y+1, favorite_number) && !visited[(y+1) as usize][x as usize] {
            queue.push((x, y+1, steps + 1));
        }
    }
}
