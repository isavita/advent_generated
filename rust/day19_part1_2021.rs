
use std::collections::{HashMap, HashSet};
use std::fs;

#[derive(Clone)]
struct Scanner {
    number: usize,
    x: i32,
    y: i32,
    z: i32,
    relative_coords: Vec<(i32, i32, i32)>,
    absolute_coords: Vec<(i32, i32, i32)>,
    absolute_coords_map: HashMap<(i32, i32, i32), bool>,
    rotations: Vec<Vec<(i32, i32, i32)>>,
}

impl Scanner {
    fn new(number: usize, coords: Vec<(i32, i32, i32)>) -> Scanner {
        let mut scanner = Scanner {
            number,
            x: 0,
            y: 0,
            z: 0,
            relative_coords: coords,
            absolute_coords: Vec::new(),
            absolute_coords_map: HashMap::new(),
            rotations: Vec::new(),
        };
        scanner.fill_rotations();
        scanner
    }

    fn fill_absolute_coords_map(&mut self) {
        for ac in &self.absolute_coords {
            self.absolute_coords_map.insert(*ac, true);
        }
    }

    fn fill_rotations(&mut self) {
        let mut rotations: Vec<Vec<(i32, i32, i32)>> = Vec::new();
        let mut six_rotations: Vec<Vec<(i32, i32, i32)>> = Vec::new();

        let pos_x = self.relative_coords.clone();
        let mut dir2 = Vec::new();
        let mut dir3 = Vec::new();
        let mut dir4 = Vec::new();
        let mut dir5 = Vec::new();
        let mut dir6 = Vec::new();

        for &(x, y, z) in &pos_x {
            dir2.push((x, -y, -z));
            dir3.push((x, -z, y));
            dir4.push((-y, -z, x));
            dir5.push((-x, -z, -y));
            dir6.push((y, -z, -x));
        }

        six_rotations.extend(vec![pos_x, dir2, dir3, dir4, dir5, dir6]);

        for rotation in six_rotations {
            let mut r2 = Vec::new();
            let mut r3 = Vec::new();
            let mut r4 = Vec::new();

            for &(x, y, z) in &rotation {
                r2.push((-y, x, z));
                r3.push((-x, -y, z));
                r4.push((y, -x, z));
            }
            rotations.extend(vec![rotation, r2, r3, r4]);
        }
        self.rotations = rotations;
    }
}

fn find_absolute_coords_for_scanner(
    undet: &Scanner,
    settled: &[Scanner],
) -> Option<Scanner> {
    for rotated_coords in &undet.rotations {
        for set_scanner in settled {
            for &abs_coord in &set_scanner.absolute_coords {
                for &relative_coord in rotated_coords {
                    let diff = (
                        abs_coord.0 - relative_coord.0,
                        abs_coord.1 - relative_coord.1,
                        abs_coord.2 - relative_coord.2,
                    );
                    let unsettled_absolute_coords: Vec<(i32, i32, i32)> =
                        rotated_coords
                            .iter()
                            .map(|&(x, y, z)| (diff.0 + x, diff.1 + y, diff.2 + z))
                            .collect();

                    let mut matching_count = 0;
                    for &ac in &unsettled_absolute_coords {
                        if set_scanner.absolute_coords_map.contains_key(&ac) {
                            matching_count += 1;
                        }
                    }

                    if matching_count >= 12 {
                        let mut updated_scanner = undet.clone();
                        updated_scanner.relative_coords = rotated_coords.clone();
                        updated_scanner.absolute_coords = unsettled_absolute_coords;
                        updated_scanner.fill_absolute_coords_map();
                        updated_scanner.x = diff.0;
                        updated_scanner.y = diff.1;
                        updated_scanner.z = diff.2;
                        return Some(updated_scanner);
                    }
                }
            }
        }
    }
    None
}

fn parse_input(input_data: &str) -> Vec<Scanner> {
    let mut scanners = Vec::new();
    for raw_scanner in input_data.split("\n\n") {
        let lines: Vec<&str> = raw_scanner.split('\n').collect();
        let number = lines[0].split_whitespace().nth(2).unwrap().parse().unwrap();
        let mut coords = Vec::new();
        for line in &lines[1..] {
            let nums: Vec<i32> = line.split(',').map(|s| s.parse().unwrap()).collect();
            coords.push((nums[0], nums[1], nums[2]));
        }
        scanners.push(Scanner::new(number, coords));
    }
    scanners
}

fn main() {
    let input_data = fs::read_to_string("input.txt").unwrap();
    let input_data = input_data.trim();

    let mut scanners = parse_input(input_data);

    let mut settled = vec![scanners.remove(0)];
    settled[0].absolute_coords = settled[0].relative_coords.clone();
    settled[0].fill_absolute_coords_map();

    let mut undetermined = scanners;

    while !undetermined.is_empty() {
        let mut i = 0;
        while i < undetermined.len() {
            if let Some(updated_scanner) =
                find_absolute_coords_for_scanner(&undetermined[i], &settled)
            {
                settled.push(updated_scanner);
                undetermined.remove(i);
                break;
            }
            i += 1;
        }
    }

    let mut all_beacons = HashSet::new();
    for s in &settled {
        for &coord in &s.absolute_coords {
            all_beacons.insert(coord);
        }
    }

    println!("{}", all_beacons.len());
}
