
import os

fn calculate_new_fuel(current int, target int) int {
    diff := if current > target { current - target } else { target - current }
    return diff * (diff + 1) / 2
}

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    mut positions := []int{}
    for part in data.split(',') {
        positions << part.int()
    }
    positions.sort()
    min_pos := positions[0]
    max_pos := positions[positions.len - 1]
    mut min_fuel := 2147483647
    for target in min_pos .. max_pos + 1 {
        mut fuel := 0
        for pos in positions {
            fuel += calculate_new_fuel(pos, target)
        }
        if fuel < min_fuel {
            min_fuel = fuel
        }
    }
    println(min_fuel)
}
