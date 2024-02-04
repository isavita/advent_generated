
fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let map: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();

    let slopes = vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
    let mut result = 1;

    for (right, down) in slopes {
        let mut trees_encountered = 0;
        let mut x = 0;
        let mut y = 0;

        while y < map.len() {
            if map[y][x % map[0].len()] == '#' {
                trees_encountered += 1;
            }
            x += right;
            y += down;
        }

        result *= trees_encountered;
    }

    println!("{}", result);
}
