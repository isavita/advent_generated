
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let coordinates: Vec<(i32, i32)> = input.lines()
        .map(|line| {
            let parts: Vec<i32> = line.split(", ")
                .map(|x| x.parse().unwrap())
                .collect();
            (parts[0], parts[1])
        })
        .collect();

    let max_x = coordinates.iter().map(|(x, _)| *x).max().unwrap();
    let max_y = coordinates.iter().map(|(_, y)| *y).max().unwrap();

    let mut areas = vec![0; coordinates.len()];
    let mut infinite = vec![false; coordinates.len()];

    for x in 0..=max_x {
        for y in 0..=max_y {
            let mut min_distance = std::i32::MAX;
            let mut closest = 0;
            let mut equal_distance = false;

            for (i, (cx, cy)) in coordinates.iter().enumerate() {
                let distance = (x - cx).abs() + (y - cy).abs();

                if distance < min_distance {
                    min_distance = distance;
                    closest = i;
                    equal_distance = false;
                } else if distance == min_distance {
                    equal_distance = true;
                }
            }

            if x == 0 || y == 0 || x == max_x || y == max_y {
                infinite[closest] = true;
            }

            if !equal_distance {
                areas[closest] += 1;
            }
        }
    }

    let result = areas.iter().zip(infinite.iter())
        .filter_map(|(&area, &inf)| if !inf { Some(area) } else { None })
        .max()
        .unwrap();

    println!("{}", result);
}
