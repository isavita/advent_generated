fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let favorite_number: i32 = input.trim().parse().unwrap();
    
    fn is_open_space(x: i32, y: i32, favorite_number: i32) -> bool {
        let sum = x*x + 3*x + 2*x*y + y + y*y + favorite_number;
        let count = sum.count_ones();
        count % 2 == 0
    }

    let mut visited: std::collections::HashSet<(i32, i32)> = std::collections::HashSet::new();
    let mut queue: std::collections::VecDeque<(i32, i32, i32)> = std::collections::VecDeque::new();
    
    queue.push_back((1, 1, 0));
    visited.insert((1, 1));
    
    let mut part1_answer = 0;
    let mut part2_answer = 0;
    
    while let Some((x, y, steps)) = queue.pop_front() {
        if x == 31 && y == 39 {
            part1_answer = steps;
        }
        
        if steps <= 50 {
            part2_answer += 1;
            for (dx, dy) in &[(0, 1), (0, -1), (1, 0), (-1, 0)] {
                let new_x = x + dx;
                let new_y = y + dy;
                if new_x >= 0 && new_y >= 0 && is_open_space(new_x, new_y, favorite_number) && !visited.contains(&(new_x, new_y)) {
                    queue.push_back((new_x, new_y, steps + 1));
                    visited.insert((new_x, new_y));
                }
            }
        }
    }
    
    println!("{}", part1_answer);
    println!("{}", part2_answer);
}