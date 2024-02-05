
use std::fs;
use std::io;
use std::num::ParseIntError;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Step 1: Read input
    let data = fs::read_to_string("input.txt")?;
    let target: i32 = data.trim().parse()?;

    // Step 2: Find the square
    let side_length = f64::from(target).sqrt().ceil() as i32;
    let side_length = if side_length % 2 == 0 { side_length + 1 } else { side_length };

    // Step 3: Find distance to the nearest middle point
    let max_value = side_length.pow(2);
    let steps_from_edge = (side_length - 1) / 2;
    let mut distance_to_middle = i32::MAX;

    for i in 0..4 {
        let middle_point = max_value - steps_from_edge - (side_length - 1) * i;
        let distance = (target - middle_point).abs();
        if distance < distance_to_middle {
            distance_to_middle = distance;
        }
    }

    // Step 4: Calculate Manhattan Distance
    let manhattan_distance = steps_from_edge + distance_to_middle;

    println!("{}", manhattan_distance);

    Ok(())
}
