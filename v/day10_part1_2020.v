import os

fn main() {
    mut adapters := []int{}
    input := os.read_file('input.txt') or { panic(err) }
    for line in input.split('\n') {
        if line.trim_space() != '' {
            adapters << line.int()
        }
    }

    adapters.sort()
    adapters.prepend(0) // Charging outlet
    adapters << (adapters.last() + 3) // Device's built-in adapter

    mut one_jolt_diff := 0
    mut three_jolt_diff := 0

    for i in 1 .. adapters.len {
        diff := adapters[i] - adapters[i - 1]
        match diff {
            1 { one_jolt_diff++ }
            3 { three_jolt_diff++ }
            else {}
        }
    }

    println(one_jolt_diff * three_jolt_diff)
}