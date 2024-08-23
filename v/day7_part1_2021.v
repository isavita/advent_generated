import os
import math

fn main() {
    positions := os.read_file('input.txt') or { panic(err) }
    crabs := positions.split(',').map(it.int())

    mut min_fuel := int(1e9)
    for pos in crabs {
        mut fuel := 0
        for crab in crabs {
            fuel += math.abs(crab - pos)
        }
        if fuel < min_fuel {
            min_fuel = fuel
        }
    }
    println(min_fuel)
}