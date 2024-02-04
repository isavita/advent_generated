use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = input.lines().collect();
    
    let depth: i32 = lines[0].split_whitespace().last().unwrap().parse().unwrap();
    let target_coords: Vec<i32> = lines[1].split_whitespace().last().unwrap().split(',').map(|x| x.parse().unwrap()).collect();
    
    let mut cave = vec![vec![0; target_coords[0] as usize + 1]; target_coords[1] as usize + 1];
    
    for y in 0..=target_coords[1] {
        for x in 0..=target_coords[0] {
            let geo_index = if (x == 0 && y == 0) || (x == target_coords[0] && y == target_coords[1]) {
                0
            } else if y == 0 {
                x * 16807
            } else if x == 0 {
                y * 48271
            } else {
                cave[y as usize - 1][x as usize] * cave[y as usize][x as usize - 1]
            };
            
            cave[y as usize][x as usize] = (geo_index + depth) % 20183;
        }
    }
    
    let risk_level: i32 = cave.iter().flatten().map(|&x| x % 3).sum();
    
    println!("{}", risk_level);
}