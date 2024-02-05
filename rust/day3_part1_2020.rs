
fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let map: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();
    
    let mut trees = 0;
    let mut x = 0;
    
    for y in 0..map.len() {
        if map[y][x % map[y].len()] == '#' {
            trees += 1;
        }
        x += 3;
    }
    
    println!("{}", trees);
}
