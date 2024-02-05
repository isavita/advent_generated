
use std::fs;

fn manhattan_distance(x1: i32, y1: i32, x2: i32, y2: i32) -> i32 {
    (x1 - x2).abs() + (y1 - y2).abs()
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let coordinates: Vec<(i32, i32)> = input.lines()
        .map(|line| {
            let parts: Vec<i32> = line.split(", ")
                .map(|part| part.parse().unwrap())
                .collect();
            (parts[0], parts[1])
        })
        .collect();

    let max_x = coordinates.iter().map(|(x, _)| *x).max().unwrap();
    let max_y = coordinates.iter().map(|(_, y)| *y).max().unwrap();

    let mut closest_counts = vec![0; coordinates.len()];
    let mut region_size = 0;

    for x in 0..=max_x {
        for y in 0..=max_y {
            let total_distance: i32 = coordinates.iter()
                .map(|(cx, cy)| manhattan_distance(x, y, *cx, *cy))
                .sum();

            if total_distance < 10000 {
                region_size += 1;
            }

            let mut closest_distance = i32::MAX;
            let mut closest_index = 0;
            let mut tied = false;

            for (i, (cx, cy)) in coordinates.iter().enumerate() {
                let distance = manhattan_distance(x, y, *cx, *cy);

                if distance < closest_distance {
                    closest_distance = distance;
                    closest_index = i;
                    tied = false;
                } else if distance == closest_distance {
                    tied = true;
                }
            }

            if !tied {
                closest_counts[closest_index] += 1;
            }
        }
    }

    let largest_area = closest_counts.iter().max().unwrap();
    println!("{}", largest_area);

    println!("{}", region_size);
}
